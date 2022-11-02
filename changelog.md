0.0.10.3 2022-11-02
===================
- upgrade flake-build-utils to 1.0.0.3

0.0.10.2 2022-11-01
==================
- add flake
- use ghc-8.10.7 for tfmt

0.0.10.1 2022-04-07
==================
- upgrade dependencies

0.0.10.0 2022-01-16
===================
- use AsMapDupKeyError for fromListDupsE and derivatives

0.0.9.0 2022-01-08
==================
- add throwAsMapDupKeyError, ToMapDupKeyError( toMapDupKeyError )

0.0.8.0 2022-01-07
==================
- export AsMapDupKeyError

0.0.7.0 2022-01-06
==================
- add AsMapDupKeyError; make Exception,Printable,HasCallstack instances of MapDupKeyError
- add Exception instance of RepeatedKeyError

0.0.6.0 2021-12-02
==================
- fromList uses AsRepeatedKeyError

0.0.5.0 2021-12-02
==================
- add AsRepeatedKeyError, HasCallStack to RepeatedKeyError

0.0.4.0 2021-06-30
==================
- add (<+), (+>), (⨭), (⨮) to Insert

0.0.3.0 2021-06-29
==================
- add ContainersPlus.Insert

0.0.2.0 2021-06-15
==================
- add ContainersPlus.Member

0.0.1.1 2021-06-13
==================
- add tests for MapUtils (from fluffy)

0.0.1.0 2021-06-12
==================
- add MapUtils.hs (also from fluffy)

0.0.0.0 2021-06-12
==================
- Map.hs factored out from fluffy

