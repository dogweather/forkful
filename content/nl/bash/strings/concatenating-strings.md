---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:00.105512-07:00
description: "Het samenvoegen van strings in Bash betekent het aan elkaar plakken\
  \ van twee of meer tekststukken. Programmeurs doen dit om commando's op te bouwen,\u2026"
lastmod: '2024-03-13T22:44:50.971582-06:00'
model: gpt-4-0125-preview
summary: Het samenvoegen van strings in Bash betekent het aan elkaar plakken van twee
  of meer tekststukken.
title: Samenvoegen van strings
weight: 3
---

## Hoe te:
Hier is de snelle manier om je strings in Bash gezellig dicht bij elkaar te brengen:

```Bash
# Samenvoegen door strings naast elkaar te plaatsen
greeting="Hallo, "
name="wereld!"
welcome=$greeting$name
echo $welcome  # Geeft uit: Hallo, wereld!

# Krullende haakjes gebruiken voor duidelijkheid
version="versie"
number=1
full_version=${version}_${number}
echo $full_version  # Geeft uit: versie_1

# Samenvoegen met variabelen en literalen
timestamp=$(date +%Y%m%d)  # Haalt de huidige datum in JJJJMMDD-formaat op
filename="backup_${timestamp}.tar.gz"
echo $filename  # Geeft uit: backup_20230315.tar.gz
```

## Diepere Duik
Terug in de dagen voordat GUI's het land regeerden, waren commandoregels en scripts de koningen van computerinteractie. Het samenvoegen van strings is altijd essentieel geweest, omdat het dynamische commando- en bestandsmanipulatie mogelijk maakt.

Een historisch alternatief is het `expr` commando, dat nu aanvoelt als een relikwie:

```Bash
older_way=$(expr $greeting $name)
```

Maar Bash zei: "Wie heeft die last nodig?" en maakte het natuurlijker. Hoe? Nou, Bash behandelt strings alsof ze gezellige vrienden zijn: zet ze naast elkaar en ze zullen samenknuffelen tot één lange string.

Onder de motorkap behandelt Bash dit zonder enige speciale functie of syntax voor samenstelling. De woorden of variabelen vloeien gewoon samen. Echter, als je variabelen hebt die kunnen beginnen met een nummer of een underscore, dan zou je ze meestal omwikkelen met krullende haakjes om verwarring met andere variabelenamen te voorkomen.

Er is echter een addertje onder het gras: spaties zijn belangrijk. Als je niet oppast, kun je eindigen met onbedoelde gaten of een samengedrukt zootje.

Een huidig alternatief is het gebruik van de `printf` functie, die je meer controle biedt over de opmaak:

```Bash
printf -v full_greeting "%s%s" "$greeting" "$name"
echo $full_greeting  # Geeft uit: Hallo, wereld!
```

## Zie Ook
- [GNU Bash-handleiding](https://www.gnu.org/software/bash/manual/) voor de ins en outs van alles wat met BASH te maken heeft.
- [Geavanceerde Bash-scriptinggids](https://tldp.org/LDP/abs/html/) voor scriptacrobatiek en meer voorbeelden.
