---
title:                "Skriva tester"
date:                  2024-01-19
html_title:           "Arduino: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skriva tester är att koda kontroller för att verifiera att din kod fungerar som förväntat. Programmerare gör detta för att förebygga fel, säkerställa kodkvalitet och underlätta framtida underhåll.

## Hur gör man:
Gleams testsystem bygger på assertions. Se följande exempel:

```gleam
import gleam/expect
import my_module

pub fn my_test() {
  expect.equal(my_module.my_function(), ExpectedValue)
}
```

Kör dina tester med `rebar3 eunit`.

## Fördjupning:
Tester i Gleam är starkt influerade av Erlangs testsystem eunit. Alternativ som Common Test finns också men är tyngre. Klara, enkelhet och funktionssäkerhet präglar Gleams testfilosofi. Det handlar om att snabbt kunna köra tester utan komplicerad konfiguration.

## Se även:
- 'Erlang and OTP in Action' för insikter i Erlang testning.
- Forumdiskussioner på [Gleam Users](https://github.com/gleam-lang/gleam/discussions) om bästa testpraxis.
