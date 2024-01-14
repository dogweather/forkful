---
title:    "Gleam: Å få gjeldende dato"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Å få den nåværende datoen kan være nyttig i mange programmeringsscenarier, for eksempel når du vil vise datoen til brukeren, beregne alder eller registrere tidspunktet for en hendelse.

## Hvordan

For å få den nåværende datoen i Gleam, kan du bruke det innebygde biblioteket `Time`.

```Gleam
import Time

let current_date = Time.now()
```

Dette vil gi deg en `Time.Date`-struktur som inneholder informasjon om den nåværende datoen, inkludert dag, måned, år og dag i uken. Du kan også angi en bestemt tidssone ved å legge til et argument i `now()`-funksjonen.

```Gleam
let current_date = Time.now("Europe/Oslo")
```

Du kan deretter bruke disse verdiene som du vil i koden din. For å vise datoen til brukeren kan du for eksempel bruke string-interpolering.

```Gleam
let year = current_date.year
let month = current_date.month
let day = current_date.day

let message = "Dagens dato er #{day}.#{month}.#{year}."

IO.print(message)
```

Dette vil skrive ut følgende:

```
Dagens dato er 15.03.2021.
```

Du kan også bruke `Time.Date`-strukturen til å utføre beregninger, slik som å finne alderen til en person.

```Gleam
import Time.Date

let birthday = Time.Date.new(1995, 11, 19)
let current_date = Time.now()
let age = Time.Date.years_between(current_date, birthday)

IO.print("Min alder er #{age}.")
```

Dette vil skrive ut alderen din basert på den nåværende datoen.

## Dype dykk

Å få den nåværende datoen kan virke enkelt, men det er faktisk mange aspekter ved tid og dato som kan være kompliserte. For eksempel må man ta hensyn til forskjellige tidszoner, skuddår og dag-lyssparingstid. Det er viktig å forstå disse konseptene for å kunne jobbe med datoer og tider nøyaktig i programmering.

## Se også

- [Official Gleam Documentation on Time Library](https://gleam.run/lib/time/)
- [Gleam GitHub Repository](https://github.com/gleam-lang/gleam)
- [Awesome Gleam - A curated list of resources for Gleam programming language](https://github.com/razum2um/awesome-gleam)