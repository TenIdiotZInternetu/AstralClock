# AstralClock
 
Semester project for the Nonprocedural programming course at Charles University.  

This application simulates the Astronomical Clock in Prague. The time shown is based on the mechanical working of the clock, not real-world astronomical measurements. 

## How to use

All you need is GHCi interpreter. Download astralclock.hs, and open it using GHCi. Then use any of the clock functions below.

`now [Clock]` - Shows what can be seen on the clock at current UTC Time  
`inTime [Clock] [TimeDifference]` - Shows what will be on the clock in specified amount of time  
`fromUtc [Clock] [UTC]` - Show what will be on the clock in specific UTC time 

`[Clock]` is a custom class implemented in this program. Most commonly you would want to use the `AstronomicalClock`, which includes all other defined Clocks. So result of:

```
ghci> now AstronomicalClock
```

should look like this:

```
==[[======= Prague Astronomical Clock =======]]==
CET Clock -> VI
Old Czech Time -> 23
Sidereal time -+ IV
Babylonian clock -* 12
-- + --------------------------------------- + --
Moon Phase: L 44.98%
-- + --------------------------------------- + --
Celestial positions of Sun
Azimuth: 90.54° (OCCASVS)
Time of rise: 04:56:24
Time of set: 17:03:35
-- + --------------------------------------- + --
Celestial positions of Moon
Azimuth: 149.0° (Astronomical Night)
Time of rise: 03:45:33
Time of set: 18:14:26
=================================================
```

`[TimeDifference]` is in seconds, but helping functions `minuteDuration`, `hourDuration`, `dayDuration` are defined for more comfortable use. For example:

```
ghci> inTime CETClock (4*hourDuration + 35*minutedDuration)
```

If you are comfortable with using the built-in Data.Time module, you can enter the desired UTC directly:


```
ghci> fromUtc BabylonianClock $ UTCTime (fromGregorian 2030 21 12) 0
```

## Clocks

There are numerous types of time and astronomical measuring devices, collectively called clocks.

#### CETClock 
Central European Time, UTC +1.   
Shown in Roman numerals from I to XII on the inner dial, pointed at by the hand.  
This clock doesn't account for daylight savings time.

#### OldCzechClock
[Old Czech Time](https://cs.wikipedia.org/wiki/Vlašské_hodiny)  
Divides day into 24 equal parts, beginning at the sunset.  
Shown in Gothic numerals from 1 to 24 on the outer dial, pointed at by the hand.  

#### SiderealClock
[Sidereal time](https://cs.wikipedia.org/wiki/Vlašské_hodiny)  
Defines day as time it takes for Earth to make one full revolution around its axis, which is 23h 56m 4s. In the span of year, the difference between these times makes a full day.  

Shown in Roman numerals from I to XII on the inner dial, pointed by the little star. It revolves at the same rate as the zodiac gear.

#### BabylonianClock
Babylonian time (or [Planetary hours](https://en.wikipedia.org/wiki/Planetary_hours#Table_of_hours)), 

Devides day into 12 equal parts, starting at sunrise and ending at sunrise. During night the time is not shown on the clock.
Shown in Arabian numerals from 1 to 12 in golden sections of the inner dial, told by the symbol of Sun lying in a certain section

#### MoonPhase
Moon Phase  
Shown on the small moon model on the moon hand,
The model is actually half-white, half-black, and revolves around its axis in the opposite direction of the hand.

This clock shows more values  
1. Whether it is illuminated from the left or the right, when looking from the center of the dial towards the moon.
2. Percentage of the visible surface illuminated.
3. In certain days it also shows the current named moon phase: `Full Moon`, `Last Quarter`, `New Moon` and `First Quarter`

#### CelestialPosition
Not really a clock, but a collection of interesting informations about the position of Sun and Moon.  
Using this clock requires additional argument, Either `Sun` or `Moon`, since computations for both celestial bodies are the same. For example:

```
ghci> inTime (CelestialPosition Moon) (2*hourDuration)
```

This clock shows many values:
1. Celestial body computed
2. Azimuth - The angle the celestial body makes with the zenith. Zenith is the direction perpendicular to ground, sun reaches zenith at noon
3. Occlusion - Colors on the inner dial represent how much is celestial body occluded by the Earth's horizon. Both Sun and Moon travel through all of its parts during day, but its easier to describe with the position of the Sun.  
Blue part of the dial represents day, when the sun is clearly visible on the sky. The Orange part represents sunrise (AVRORA) and sunset (CREPASCVS), and is bounded by the horizon line.  
Just above the horizon line is dawn (ORTVS) and twilight (OCCASVS), at the same position as 1 and 12 of the Babylonian clock.  
18 Degrees below the the horizon line is situated the black part of the dial, it represents astronomical night, time at which the stars are visible the most
4. Time of rise - Time at which the body rises above the horizon line, in UTC
5. Time of set - Time at which the body sets below the horizon line, in UTC

#### AstronomicalClock
Culmination of all the clocks above.

## Resources used
This application wouldn't be possible without learning about the mecanical workings of the clock and the geometrical constructions of its astrolabe. These resources were essential:

* [Prague Astronomical Clock by Václav Rosický](https://www.digitalniknihovna.cz/mlp/view/uuid:BE7D9770-7DB4-11DD-A925-000D606F5DC6?page=uuid:232283c0-7db5-11dd-b740-000d606f5dc6) - Technical details of the clock's construction from the early 20th century.
* [orloj.eu](https://orloj.eu/en/astro_cifernik.htm) - Explanation of the clock's dials and the construction of the astrolabe.
* [orloj.eu Simulator](https://orloj.eu/sim/index.php) - Online simulator I used to compare the real-time values.

This application doesn't portray 100% accurate state of the clock at every time, but that wasn't the main purpose of this exercise. But it's still a very good approximation.