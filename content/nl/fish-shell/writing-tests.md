---
title:                "Tests Schrijven"
date:                  2024-01-28T22:13:21.700379-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tests Schrijven"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/fish-shell/writing-tests.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Tests schrijven is kleine controles creëren om ervoor te zorgen dat je code zich gedraagt zoals verwacht. Programmeurs schrijven tests om bugs vroeg te vangen, tijd te besparen en code betrouwbaar te houden terwijl deze verandert.

## Hoe:
Fish heeft geen ingebouwd testframework, maar je kunt `fisher` gebruiken om er een te installeren zoals `Fishtape`. Hier is een eenvoudige test met `Fishtape`:

```fish
# Installeer eerst Fishtape
fisher install jorgebucaran/fishtape

# Maak een testbestand, `test_my_function.fish`
function test_my_function
    echo "My_function tests uitvoeren"

    # Testcase
    my_function argument
    echo $status | fishtape
end

# Voer je testbestand uit in Fish Shell
fishtape test_my_function.fish
```

Een voorbeeld van de output kan er zo uitzien:

```
TAP versie 13
ok 1 my_function met argument

1..1
# tests 1
# pass  1

# ok
```

## Diepere Duik
Fish shell is ontstaan in 2005, lang na Bash. Vanaf het begin ging het om slimme functies en gebruiksvriendelijkheid. In tegenstelling tot Bash, wordt het niet geleverd met een stapel testtools. Daar komen derde partijen zoals `Fishtape` om de hoek kijken, waarmee de ontbrekende testfunctionaliteit aan Fish wordt toegevoegd. Onthoud, Fish-scripts kunnen net als elk ander script worden getest - door output en exitstatussen te controleren - maar met `Fishtape` krijg je TAP-conforme output die gemakkelijker te gebruiken is in CI/CD-pipelines en met testharnassen.

## Zie Ook
Bekijk deze bronnen om dieper in te gaan op Fish Shell en `Fishtape`:
- [Officiële Fish-documentatie](https://fishshell.com/docs/current/index.html)
- [Fishtape op GitHub](https://github.com/jorgebucaran/fishtape)
- [Fisher Plugin Manager](https://github.com/jorgebucaran/fisher)
