---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:12.715245-07:00
description: 'Hoe te: In Ruby wikkel je je variabele of expressie in `#{}` en plaats
  je het waar je het wilt hebben in een string met dubbele aanhalingstekens. Zo dus.'
lastmod: '2024-03-13T22:44:51.320578-06:00'
model: gpt-4-0125-preview
summary: In Ruby wikkel je je variabele of expressie in `#{}` en plaats je het waar
  je het wilt hebben in een string met dubbele aanhalingstekens.
title: Een string interpoleren
weight: 8
---

## Hoe te:
In Ruby wikkel je je variabele of expressie in `#{}` en plaats je het waar je het wilt hebben in een string met dubbele aanhalingstekens. Zo dus:

```Ruby
name = "Jesse"
greeting = "Hey daar, #{name}!"
puts greeting # => Hey daar, Jesse!
```

Je bent niet beperkt tot alleen variabelen; elke Ruby-code kan erin:

```Ruby
price_per_kg = 5
quantity = 2
puts "Je totaal is: $#{price_per_kg * quantity}" # => Je totaal is: $10
```

Onthoud, enkele aanhalingstekens werken niet:

```Ruby
puts 'Hey daar, #{name}!' # => Hey daar, \#{name}!
```

## Diepgaand
Vroeger zouden we strings en variabelen samenvoegen met `+` of `<<`, wat snel rommelig werd.

```Ruby
email = "gebruiker" + "@" + "voorbeeld.com"
```

Voer stringinterpolatie in Ruby in, een verfijndere manier om tekst met code te samenvoegen. Ruby evalueert wat er binnen `#{}` staat en zet dit automatisch om naar een string. Overweeg het werk dat het bespaart van het converteren en samenvoegen van strings:

```Ruby
"pi is ongeveer #{Math::PI.round(2)}"
```

Ruby is niet uniek; veel talen hebben hun eigen variant van deze handige functie. Maar let op: in tegenstelling tot sommige talen, reserveert Ruby deze magie strikt voor strings met dubbele aanhalingstekens en bepaalde andere gevallen (zoals backticks en symbolen). Enkele aanhalingstekens geven gewoon uit wat erin zit, krullende haakjes en al.

## Zie Ook
- Ruby-documentatie over syntax: [Ruby Docs - Syntax](https://ruby-doc.org/core-3.1.2/doc/syntax/literals_rdoc.html#label-Strings)
- Een diepere kijk op stringmanipulatie: [Ruby-Doc.org - String](https://ruby-doc.org/core-3.1.2/String.html)
