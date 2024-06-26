---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:21.700379-07:00
description: "Hoe: Fish heeft geen ingebouwd testframework, maar je kunt `fisher`\
  \ gebruiken om er een te installeren zoals `Fishtape`. Hier is een eenvoudige test\
  \ met\u2026"
lastmod: '2024-03-13T22:44:51.250871-06:00'
model: gpt-4-0125-preview
summary: Fish heeft geen ingebouwd testframework, maar je kunt `fisher` gebruiken
  om er een te installeren zoals `Fishtape`.
title: Tests Schrijven
weight: 36
---

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
