{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar.Plugins.Monitors
-- Copyright   :  (c) 2010, 2011, 2012, 2013, 2017, 2018, 2019 Jose Antonio Ortega Ruiz
--                (c) 2007-10 Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- The system monitor plugin for Xmobar.
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.Monitors where

import Xmobar.Run.Exec

import Xmobar.Plugins.Monitors.Common (runM, runMD)
#ifdef WEATHER
import Xmobar.Plugins.Monitors.Weather
#endif
import Xmobar.Plugins.Monitors.Net
import Xmobar.Plugins.Monitors.Mem
import Xmobar.Plugins.Monitors.Swap
import Xmobar.Plugins.Monitors.Cpu
import Xmobar.Plugins.Monitors.MultiCpu
import Xmobar.Plugins.Monitors.Batt
import Xmobar.Plugins.Monitors.Bright
import Xmobar.Plugins.Monitors.Thermal
import Xmobar.Plugins.Monitors.ThermalZone
import Xmobar.Plugins.Monitors.CpuFreq
import Xmobar.Plugins.Monitors.CoreTemp
import Xmobar.Plugins.Monitors.MultiCoreTemp
import Xmobar.Plugins.Monitors.Disk
import Xmobar.Plugins.Monitors.Top
import Xmobar.Plugins.Monitors.Uptime
import Xmobar.Plugins.Monitors.CatInt
#ifdef UVMETER
import Xmobar.Plugins.Monitors.UVMeter
#endif
#ifdef IWLIB
import Xmobar.Plugins.Monitors.Wireless
#endif
#ifdef LIBMPD
import Xmobar.Plugins.Monitors.MPD
import Xmobar.Plugins.Monitors.Common (runMBD)
#endif
#ifdef ALSA
import Xmobar.Plugins.Monitors.Volume
import Xmobar.Plugins.Monitors.Alsa
#endif
#ifdef MPRIS
import Xmobar.Plugins.Monitors.Mpris
#endif

data Monitors = Network      Interface   Args Rate
              | DynNetwork               Args Rate
              | BatteryP     Args        Args Rate
              | BatteryN     Args        Args Rate Alias
              | Battery      Args        Rate
              | DiskU        DiskSpec    Args Rate
              | DiskIO       DiskSpec    Args Rate
              | Thermal      Zone        Args Rate
              | ThermalZone  ZoneNo      Args Rate
              | Memory       Args        Rate
              | Swap         Args        Rate
              | Cpu          Args        Rate
              | MultiCpu     Args        Rate
              | Brightness   Args        Rate
              | CpuFreq      Args        Rate
              | CoreTemp     Args        Rate
              | MultiCoreTemp Args       Rate
              | TopProc      Args        Rate
              | TopMem       Args        Rate
              | Uptime       Args        Rate
              | CatInt       Int FilePath Args Rate
#ifdef WEATHER
              | Weather      Station     Args Rate
              | WeatherX     Station SkyConditions Args Rate
#endif
#ifdef UVMETER
              | UVMeter      Station     Args Rate
#endif
#ifdef IWLIB
              | Wireless Interface  Args Rate
#endif
#ifdef LIBMPD
              | MPD      Args       Rate
              | AutoMPD  Args
#endif
#ifdef ALSA
              | Volume   String     String Args Rate
              | Alsa     String     String Args
#endif
#ifdef MPRIS
              | Mpris1   String     Args Rate
              | Mpris2   String     Args Rate
#endif
                deriving (Show,Read,Eq)

type Args      = [String]
type Program   = String
type Alias     = String
type Station   = String
type SkyConditions = [(String, String)]
type Zone      = String
type ZoneNo    = Int
type Interface = String
type Rate      = Int
type DiskSpec  = [(String, String)]

instance Exec Monitors where
#ifdef WEATHER
    alias (Weather s _ _) = s
    alias (WeatherX s _ _ _) = s
#endif
    alias (Network i _ _) = i
    alias (DynNetwork _ _) = "dynnetwork"
    alias (Thermal z _ _) = z
    alias (ThermalZone z _ _) = "thermal" ++ show z
    alias (Memory _ _) = "memory"
    alias (Swap _ _) = "swap"
    alias (Cpu _ _) = "cpu"
    alias (MultiCpu _ _) = "multicpu"
    alias (Battery _ _) = "battery"
    alias BatteryP {} = "battery"
    alias (BatteryN _ _ _ a)= a
    alias (Brightness _ _) = "bright"
    alias (CpuFreq _ _) = "cpufreq"
    alias (TopProc _ _) = "top"
    alias (TopMem _ _) = "topmem"
    alias (CoreTemp _ _) = "coretemp"
    alias (MultiCoreTemp _ _) = "multicoretemp"
    alias DiskU {} = "disku"
    alias DiskIO {} = "diskio"
    alias (Uptime _ _) = "uptime"
    alias (CatInt n _ _ _) = "cat" ++ show n
#ifdef UVMETER
    alias (UVMeter s _ _) = "uv " ++ s
#endif
#ifdef IWLIB
    alias (Wireless i _ _) = i ++ "wi"
#endif
#ifdef LIBMPD
    alias (MPD _ _) = "mpd"
    alias (AutoMPD _) = "autompd"
#endif
#ifdef ALSA
    alias (Volume m c _ _) = m ++ ":" ++ c
    alias (Alsa m c _) = "alsa:" ++ m ++ ":" ++ c
#endif
#ifdef MPRIS
    alias (Mpris1 _ _ _) = "mpris1"
    alias (Mpris2 _ _ _) = "mpris2"
#endif
    start (Network  i a r) = startNet i a r
    start (DynNetwork a r) = startDynNet a r
    start (Cpu a r) = startCpu a r
    start (MultiCpu a r) = startMultiCpu a r
    start (TopProc a r) = startTop a r
    start (TopMem a r) = runM a topMemConfig runTopMem r
#ifdef WEATHER
    start (Weather  s   a r) = startWeather    s a r
    start (WeatherX s c a r) = startWeather' c s a r
#endif
    start (Thermal z a r) = runM (a ++ [z]) thermalConfig runThermal r
    start (ThermalZone z a r) =
      runM (a ++ [show z]) thermalZoneConfig runThermalZone r
    start (Memory a r) = runM a memConfig runMem r
    start (Swap a r) = runM a swapConfig runSwap r
    start (Battery a r) = runM a battConfig runBatt r
    start (BatteryP s a r) = runM a battConfig (runBatt' s) r
    start (BatteryN s a r _) = runM a battConfig (runBatt' s) r
    start (Brightness a r) = runM a brightConfig runBright r
    start (CpuFreq a r) = runM a cpuFreqConfig runCpuFreq r
    start (CoreTemp a r) = runM a coreTempConfig runCoreTemp r
    start (MultiCoreTemp a r) = startMultiCoreTemp a r
    start (DiskU s a r) = runM a diskUConfig (runDiskU s) r
    start (DiskIO s a r) = startDiskIO s a r
    start (Uptime a r) = runM a uptimeConfig runUptime r
    start (CatInt _ s a r) = runM a catIntConfig (runCatInt s) r
#ifdef UVMETER
    start (UVMeter s a r) = runM (a ++ [s]) uvConfig runUVMeter r
#endif
#ifdef IWLIB
    start (Wireless i a r) = runM a wirelessConfig (runWireless i) r
#endif
#ifdef LIBMPD
    start (MPD a r) = runMD a mpdConfig runMPD r mpdReady
    start (AutoMPD a) = runMBD a mpdConfig runMPD mpdWait mpdReady
#endif
#ifdef ALSA
    start (Volume m c a r) = runM a volumeConfig (runVolume m c) r
    start (Alsa m c a) = startAlsaPlugin m c a
#endif
#ifdef MPRIS
    start (Mpris1 s a r) = runM a mprisConfig (runMPRIS1 s) r
    start (Mpris2 s a r) = runM a mprisConfig (runMPRIS2 s) r
#endif
