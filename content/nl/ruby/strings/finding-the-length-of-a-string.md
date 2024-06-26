---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:28.076545-07:00
description: 'Hoe te: Ruby houdt het simpel met de `.length` methode.'
lastmod: '2024-03-13T22:44:51.325783-06:00'
model: gpt-4-0125-preview
summary: Ruby houdt het simpel met de `.length` methode.
title: De lengte van een string vinden
weight: 7
---

## Hoe te:
Ruby houdt het simpel met de `.length` methode:

```ruby
begroeting = "Hallo, wereld!"
puts begroeting.length
```

Uitvoer:

```
13
```

Of gebruik `.size`, wat hetzelfde doet:

```ruby
begroeting = "Hallo, wereld!"
puts begroeting.size
```

Uitvoer:

```
13
```

## Diepere Duik
In Ruby zijn `.length` en `.size` uitwisselbaar als het op strings aankomt; ze geven je het aantal karakters. Historisch gezien heeft Ruby zich gericht op het natuurlijker leesbaar maken van de code, wat de reden is waarom je vaak meer dan één manier vindt om hetzelfde te doen.

Intern heeft elk karakter in een string invloed op de opslaggrootte. Het kennen van dit aantal kan dus essentieel zijn voor optimalisatie, vooral met enorme hoeveelheden tekst.

Hoewel `.length` en `.size` je het aantal karakters geven, kan in sommige talen en vroegere tijden de lengte van een string verwijzen naar de grootte in bytes. Ruby, met zijn ondersteuning voor multibyte karakters via Unicode, gelijkstelt bytegrootte echter niet direct aan stringlengte vanwege karakters die mogelijk meer dan één byte innemen.

Alternatieven zoals `.bytesize` vertellen je hoeveel bytes een string inneemt, en `.chars.count` geeft je het aantal karakters door eerst de string om te zetten in een array van karakters.

Zo zou je `.bytesize` en `.chars.count` gebruiken:

```ruby
begroeting = "Hallo, wereld!"
puts begroeting.bytesize
puts begroeting.chars.count
```

Uitvoer:

```
13
13
```

## Zie Ook
- Ruby documentatie over Strings: [https://ruby-doc.org/core/String.html](https://ruby-doc.org/core/String.html)
- Een fijne inleiding tot Ruby Strings door [RubyGuides](https://www.rubyguides.com/2018/01/ruby-string-methods/): ontdek meer over wat je kunt doen met strings buiten het meten van hun grootte.
- Duik in karaktercodering en hoe het stringoperaties beïnvloedt met [dit artikel van Thoughtbot](https://thoughtbot.com/blog/its-about-time-zones#character-encoding).
