---
title:                "Avrunding av tall"
date:                  2024-01-26T03:44:44.728278-07:00
model:                 gpt-4-0125-preview
simple_title:         "Avrunding av tall"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/rounding-numbers.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Runding av tall dreier seg om å justere en verdi til nærmeste spesifiserte plass – for eksempel fra 2,56 til 3 hvis vi runder til hele tall. Programmerere gjør dette for enkelhets skyld eller for å møte visse numeriske spesifikasjoner, vanligvis for å unngå nyanser forårsaket av presisjonsfeil med flyttall eller for å gjøre utdataene brukervennlige.

## Hvordan:
I Gleam er ikke avrunding i standardbiblioteket per min siste sjekk, men her er hvordan du typisk runder et flyttall til nærmeste hele tall ved å bruke Erlang-funksjoner direkte:

```gleam
external fn erlang_round(Float) -> Int = "erlang" "round"

pub fn main() {
  let rounded = erlang_round(2.56)
  rounded // Utdata: 3
}
```

Utdata:
```
3
```

Har du en annen presisjon i tankene? Si avrunding til to desimaler? Vi trenger litt matte:

```gleam
pub fn round_to_two_places(num: Float) -> Float {
  let multiplier = 100.0
  let tmp = num * multiplier
  let round_tmp = erlang_round(tmp)
  round_tmp / multiplier
}

pub fn main() {
    round_to_two_places(2.569) // Utdata: 2.57
}
```

Utdata:
```
2.57
```

## Dypdykk
Historisk sett har avrunding av tall vært avgjørende, spesielt innen finans og vitenskapelige beregninger der presisjon og standarder betyr enormt mye. Uten avrunding ville du fått lange desimaler overalt, noe som gjør beregninger upraktiske og utsatt for feil.

I programmeringsverdenen tilbyr ulike språk ulike tilnærminger, fra innebygde funksjoner til omfattende matematikkbiblioteker. Avrunding kan involvere ulike regler – for eksempel "avrund halv opp" (den vanlige metoden) eller "avrund halvt til jevnt" (ofte brukt i finansielle beregninger for å unngå skjevhet).

Gleam, som er et ungt språk med røtter i Erlang, støtter seg på Erlangs robuste sett med numeriske funksjoner. Ettersom språket vokser, kan vi se at det introduseres native funksjoner, noe som reduserer behovet for å kalle eksterne rutiner.

## Se også
- Erlangs :math-modul for mer tallknusing: https://erlang.org/doc/man/math.html
- For bakgrunn om hvorfor avrunding kan bli vanskelig, IEEE Floating Point Standard: https://ieeexplore.ieee.org/document/8766229
- Interessert i matematikken bak dette? Sjekk "What Every Computer Scientist Should Know About Floating-Point Arithmetic": https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html
