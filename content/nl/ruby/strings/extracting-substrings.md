---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:05.193051-07:00
description: "Substringen extraheren gaat over het uithalen van specifieke stukken\
  \ tekst uit een string. Programmeurs doen dit om delen van data te manipuleren en\
  \ te\u2026"
lastmod: '2024-03-11T00:14:25.187031-06:00'
model: gpt-4-0125-preview
summary: "Substringen extraheren gaat over het uithalen van specifieke stukken tekst\
  \ uit een string. Programmeurs doen dit om delen van data te manipuleren en te\u2026"
title: Substrings extraheren
---

{{< edit_this_page >}}

## Wat & Waarom?

Substringen extraheren gaat over het uithalen van specifieke stukken tekst uit een string. Programmeurs doen dit om delen van data te manipuleren en te gebruiken—zoals gebruikersnamen uit e-mailadressen halen of datums uit timestamps parsen.

## Hoe:

Ruby maakt het extraheren van substrings eenvoudig. Laten we meteen ter zake komen:

```Ruby
str = "Hallo, Ruby Wereld!"

# Methode 1: Gebruik van array indices
substring = str[7, 4] # "Ruby"
puts substring

# Methode 2: Gebruik van de methode slice
slice = str.slice(7, 4) # "Ruby"
puts slice

# Methode 3: Reguliere expressies
match = str[/[Rr]uby/] # "Ruby"
puts match

# Methode 4: split en array toegang
split_array = str.split # standaard wordt gesplitst op witruimte
gekozen_woord = split_array[2] # "Wereld!"
puts gekozen_woord
```

Voorbeelduitvoer voor elk fragment zal respectievelijk "Ruby", "Ruby", "Ruby", "Wereld!" zijn.

## Diepe duik

Vroeger was het extraheren van substrings een uitgebreider proces. Ruby is echter geëvolueerd. Vandaag de dag heb je methoden en regex tot je beschikking.

Dit is wat er onder de motorkap gebeurt: `[7, 4]` betekent start bij het 7e teken en pak de volgende 4. `slice` is gewoon een methodische manier om hetzelfde te zeggen. Met regex is `/[Rr]uby/` alsof je zegt: "Vang een 'Ruby' of 'ruby', wat je als eerste vindt." `split` hakt de string in een array bij elke ruimte, en `[2]` kiest het derde woord—arrays beginnen bij nul, onthoud dat.

Alternatieven? Zeker, Ruby heeft ze. `partition`, `rpartition` en `match` zouden hier ook van pas kunnen komen. Elk heeft zijn geval, maar het kennen van `.slice` en regex dekt de meeste bases.

Kortom: het extraheren van substrings gaat over precieze tekstmanipulatie. Het juiste gereedschap betekent schone, effectieve code.

## Zie ook

- Ruby Docs over String: [ruby-doc.org/core-2.7.0/String.html](https://ruby-doc.org/core-2.7.0/String.html)
- Reguliere Expressies in Ruby: [ruby-doc.org/core-2.7.0/Regexp.html](https://ruby-doc.org/core-2.7.0/Regexp.html)
- Ruby Style Guide over Strings: [rubystyle.guide/#strings](https://rubystyle.guide/#strings)
