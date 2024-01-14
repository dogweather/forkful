---
title:    "Elixir: Päivämäärän saaminen"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Miksi
Tämän blogin artikkelin aiheena on päivämäärän hankkiminen Elixir-ohjelmoinnissa. Päivämäärän hankkiminen on tärkeä osa monia sovelluksia, kuten aikaleimoja, sääsovelluksia ja tilastollisia laskelmia. Jatka lukemista ja opi, kuinka voit helposti hankkia nykyisen päivämäärän Elixirillä.

## Kuinka
Päivämäärän hankkiminen Elixir-ohjelmoinnissa on helppoa. Voit käyttää Elixirin sisäistä `DateTime`-moduulia tai `Calendar`-moduulia saadaksesi nykyisen päivämäärän.

### DateTime-moduuli
Voit hankkia nykyisen päivämäärän `DateTime.utc_now/0`-funktiolla, joka palauttaa UTC-aikaa vastaavan `DateTime`-olion.

```Elixir
DateTime.utc_now()
=> #DateTime<2019-04-24 07:19:49Z>
```

Voit myös hankkia paikallisen ajan `DateTime.now/0`-funktiolla, joka palauttaa paikallisen aikavyöhykkeen mukaisen `DateTime`-olion.

```Elixir
DateTime.now()
=> #DateTime<2019-04-24 10:19:49+03:00 EEST>
```

Voit muokata päivämäärän esitysmuotoa `DateTime.to_iso8601/1`-funktiolla.

```Elixir
DateTime.to_iso8601(DateTime.now())
=> "2019-04-24T10:19:49+03:00"
```

### Calendar-moduuli
Voit myös käyttää `Calendar`-moduulia hankkimaan nykyisen päivämäärän. Voit käyttää `Date.utc_today/0`-funktiota hankkimaan nykyisen UTC-päivämäärän.

```Elixir
Date.utc_today()
=> {:ok, ~D[2019-04-24]}
```

Voit myös hankkia nykyisen paikallisen päivämäärän `Date.today/0`-funktiolla.

```Elixir
Date.today()
=> {:ok, ~D[2019-04-24]}
```

Voit muokata päivämäärän esitysmuotoa `Date.to_iso8601/2`-funktiolla.

```Elixir
Date.to_iso8601(Date.today(), [{:format, :iso8601}])
=> {:ok, "2019-04-24+03:00"}
```

## Syvemmälle
Elixir:ssä on myös muita tapoja hankkia päivämäärä, kuten käyttämällä `:calendar.universal_time/0`-funktiota, joka palauttaa nykyisen UTC-aika-timessä. Tämä voi olla hyödyllistä, kun työskentelet kansainvälisten aikavyöhykkeiden kanssa.

```Elixir
:calendar.universal_time()
=> {{2019, 4, 24}, {7, 19, 49}}
```

Voit myös käyttää `:calendar.local_time/0`-funktiota hankkimaan nykyisen paikallisen ajan timessä.

```Elixir
:calendar.local_time()
=> {{2019, 4, 24}, {10, 19, 49}}
```

## Katso myös
- [Elixir DateTime-moduuli] (https://hexdocs.pm/elixir/DateTime.html)
- [Elixir Calendar-moduuli] (https://hexdocs.pm/elixir/Calendar.html)
- [Elixirin virallinen dokumentaatio] (https://elixir-lang.org/docs.html)