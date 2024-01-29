---
title:                "Een string interpoleren"
date:                  2024-01-28T22:02:06.787585-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een string interpoleren"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/interpolating-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Stringinterpolatie is het proces van het inbedden van uitdrukkingen in string literals om nieuwe stringwaarden te creëren. Programmeurs interpoleren strings om dynamisch berichten te construeren, vaak voor gebruikersuitvoer of logboekregistratie.

## Hoe:

In Gleam is stringinterpolatie eenvoudig. Gebruik de syntax `#{}` om uitdrukkingen in strings in te voegen. Hier is een snel voorbeeld:

```gleam
fn main() {
  let name = "wereld"
  let greeting = "Hallo, #{name}!"
  greeting
}

// Dit zal uitvoeren: "Hallo, wereld!"
```

## Diepere Duik

Historisch was stringconcatenatie de norm, waarbij je handmatig strings en waarden zou samenvoegen. Het wordt snel rommelig. Interpolatie is schoner en leesbaarder.

Talen variëren in hun interpolatie syntax; Gleams `#{}` spiegelt die van Ruby en Elixir. Deze consistentie is nuttig voor mensen die tussen talen wisselen.

Onder de kap transformeert de compiler van Gleam geïnterpoleerde strings tot een reeks van stringconcatenaties voordat het wordt gecompileerd naar Erlang, de taal waarin Gleam compileert. Dus:

```gleam
"Hallo, #{name}!"
```

wordt iets zoals (in Erlang pseudo-code):

```erlang
"Hallo, " ++ name ++ "!"
```

De keuze voor interpolatie over concatenatie gaat meestal over leesbaarheid en gemak, hoewel er door compiler optimalisaties niet veel verschil in prestatie is.

## Zie Ook

- [Het Gleam Boek](https://gleam.run/book/)
- [Erlangs String module documentatie](http://erlang.org/doc/man/string.html) voor achtergrondinformatie over waar Gleam naar compileert.
