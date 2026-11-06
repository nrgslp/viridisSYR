# viridisSYR 0.2.0

* **New default scale.** The scale now runs from Syracuse navy (`#000E54`) at 0 through the viridis plasma start
  colour (`#0D0887`) at 0.05, the plasma purple (`#7E03A8`) at 0.2875 and Syracuse orange (`#F76700`) at 0.689 to the
  plasma yellow (`#F0F921`) at 1. Plasma is compressed into positions 0.05 to 1, with a short constant-speed lead-in
  from navy. Colours and key positions differ from 0.1.0 (the purple was at 0.25 and the orange at 0.673).
* The colour map is now 2001 colours (`syr.map`); `syr_build()` default `n = 2001`.
* `syr_keys()` returns five key colours (adds `viridis_start`); `syr_value_at()` accepts `"viridis_start"`.
* `syr_plasma_keys()` now reports the closest colour and the equivalent position (`equivalent_*` replaces
  `same_position_*`). The plasma start, purple and yellow are exact matches.
* Use `begin = 0.05` to start at the plasma start colour instead of navy.
# viridisSYR 0.1.0 (superseded)

* First version. The viridisSYR colour scale: Syracuse navy (`#000E54`) at 0, the viridis plasma purple
  (`#7E03A8`) at 0.25, Syracuse orange (`#F76700`) at 0.673, the plasma yellow (`#F0F921`) at 1.
* `syr()`, `syr_at()`, `syr_pal()`, `syr_keys()`, `syr_su()`, `syr_value_at()`, `syr_plasma_keys()`.
* ggplot2 scales: `scale_{colour,color,fill}_syr_{c,d}()`.
* `cam02ucs()`, `delta_e()`, `syr_metrics()`, `syr_compare()` to evaluate a scale with viridis' design criteria.
* Fixed colour map `syr.map` (1001 colours) and `syr_build()` to regenerate it.
