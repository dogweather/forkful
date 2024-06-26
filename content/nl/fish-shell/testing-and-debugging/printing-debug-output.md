---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:24.862576-07:00
description: 'Hoe te: Maak jezelf vertrouwd met `echo` - het Zwitserse zakmes voor
  output in Fish. Hier is hoe je wat debug-prints kunt toevoegen aan je shellscripts.'
lastmod: '2024-03-13T22:44:51.249938-06:00'
model: gpt-4-0125-preview
summary: Maak jezelf vertrouwd met `echo` - het Zwitserse zakmes voor output in Fish.
title: Debug-output afdrukken
weight: 33
---

## Hoe te:
Maak jezelf vertrouwd met `echo` - het Zwitserse zakmes voor output in Fish. Hier is hoe je wat debug-prints kunt toevoegen aan je shellscripts.

```Fish Shell
function greet
    set name $argv[1]
    echo "Hey, $name! Laten we debuggen."
    echo "De greet-functie wordt uitgevoerd" >&2
end

greet "Ada"
```
Voorbeelduitvoer:
```
Hey, Ada! Laten we debuggen.
De greet-functie wordt uitgevoerd
```
Standaard uitvoer (`stdout`) is het hoofdpodium van je script, maar voor debug-geklets, gebruik standaard fout (`stderr`) met `>&2`.

## Diepere duik
Terug in de tijd, toen monitoren net zo diep als breed waren, was output kostbaar. Standaard uitvoer (`stdout`) werd het zuivere, op de gebruiker gerichte kanaal, terwijl standaard fout (`stderr`) veranderde in het achterafstraatje voor alleen programmeur-gezwets, zoals debug-informatie.

In Fish zijn de standaard commando's voor output `echo`, `printf`, en `print`. De `echo` is eenvoudig en voornamelijk gebruikt voor simpele berichten en inline debug.

Je bent niet alleen aan `echo` gebonden, hoewel. Geef de voorkeur aan `printf` voor geformatteerde strings, of gebruik omleiding (`>` of `>>`) om debug-info in een bestand te dumpen voor later.

Wat betreft de implementatie, het gebruiken van `stderr` voor debug-output is een conventie uit de Unix-wereld, die helpt om het kaf (de daadwerkelijke output) van het koren (debug-ruis) te scheiden. Dit betekent dat gebruikers nog steeds de werkelijke output van je script kunnen pijplijnen zonder dat debug-geklets erin gemengd wordt.

## Zie ook
- Fish Shell Documentatie over [Commando's](https://fishshell.com/docs/current/commands.html)
- StackOverflow: Discussies en voorbeelden van [debuggen in Fish](https://stackoverflow.com/questions/tagged/fish)
- Greg's Wiki: Diepgaande info over [I/O omleiding](https://mywiki.wooledge.org/BashGuide/InputAndOutput#Redirection)
