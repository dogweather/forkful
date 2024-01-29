---
title:                "Afronden van getallen"
date:                  2024-01-28T22:06:53.136210-07:00
model:                 gpt-4-0125-preview
simple_title:         "Afronden van getallen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/rounding-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het afronden van getallen gaat over het aanpassen van een waarde naar de dichtstbijzijnde gespecificeerde plaats—zoals van 2,56 naar 3 als we afronden op hele getallen. Programmeurs doen dit voor eenvoud of om aan bepaalde numerieke specificaties te voldoen, meestal om nuances veroorzaakt door drijvende-komma precisiefouten te vermijden of om de uitvoer gebruiksvriendelijk te maken.

## Hoe:
In Gleam zit afronden niet in de standaardbibliotheek voor zover ik weet, maar hier is hoe je typisch een float afrondt naar het dichtstbijzijnde hele getal met behulp van Erlang-functies:

```gleam
external fn erlang_round(Float) -> Int = "erlang" "round"

pub fn main() {
  let rounded = erlang_round(2.56)
  rounded // Uitvoer: 3
}
```

Uitvoer:
```
3
```

Heb je een andere precisie in gedachten? Zeg, afronden op twee decimalen? We hebben een beetje wiskunde nodig:

```gleam
pub fn round_to_two_places(num: Float) -> Float {
  let multiplier = 100.0
  let tmp = num * multiplier
  let round_tmp = erlang_round(tmp)
  round_tmp / multiplier
}

pub fn main() {
    round_to_two_places(2.569) // Uitvoer: 2.57
}
```

Uitvoer:
```
2.57
```

## Diepe Duik
Historisch gezien is het afronden van getallen cruciaal geweest, met name in financiële en wetenschappelijke berekeningen waar precisie en normen enorm belangrijk zijn. Zonder afronding zou je overal lange, lastige decimale getallen krijgen, waardoor berekeningen onpraktisch en foutgevoelig worden.

In de programmeerwereld bieden verschillende talen verschillende benaderingen, van ingebouwde functies tot uitgebreide wiskundebibliotheken. Afronden kan verschillende regels omvatten - bijvoorbeeld "afronden half omhoog" (de gebruikelijke methode) of "afronden half naar even" (vaak gebruikt in financiële berekeningen om bevooroordeeldheid te vermijden).

Gleam, als een jonge taal met wortels in Erlang, leunt op Erlang's robuuste set van numerieke functies. Naarmate de taal groeit, zien we misschien inheemse functies geïntroduceerd worden, waardoor de noodzaak om externe routines aan te roepen afneemt.

## Zie Ook
- Erlang's :math module voor meer rekenwerk: https://erlang.org/doc/man/math.html
- Voor achtergrond over waarom afronden lastig kan worden, de IEEE Floating Point Standard: https://ieeexplore.ieee.org/document/8766229
- Geïnteresseerd in de wiskunde hierachter? Lees "What Every Computer Scientist Should Know About Floating-Point Arithmetic": https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html
