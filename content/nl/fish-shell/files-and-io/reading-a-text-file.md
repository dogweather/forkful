---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:50.495010-07:00
description: 'Hoe: Hier is de scoop van Fish Shell over het openen van die tekstbestanden.'
lastmod: '2024-03-13T22:44:51.264494-06:00'
model: gpt-4-0125-preview
summary: Hier is de scoop van Fish Shell over het openen van die tekstbestanden.
title: Een tekstbestand lezen
weight: 22
---

## Hoe:
Hier is de scoop van Fish Shell over het openen van die tekstbestanden:

```Fish Shell
# Lees een bestand regel voor regel
while read -la line
    echo $line
end < file.txt
```

```Fish Shell
# Toon de inhoud van een bestand direct
cat file.txt
```

Voorbeelduitvoer (van `cat`):

```plaintext
Hallo, Fish!
Gewoon door bestanden zwemmend.
```

## Diepere duik
Er was een tijd, zelfs voordat Fish Shell zijn debuut maakte rond 2005, dat het lezen van bestanden een noodzaak was. Unix shells hebben altijd gereedschappen hiervoor gehad. Waarom Fish? Het is gebruiksvriendelijk, modern, en het heeft verstandige standaardwaarden voor scripting, waardoor het een aangenaam alternatief is voor oudere shells.

De `while read`-lus is handig voor aanpassingen regel voor regel. Vergeet niet dat `read` vlaggen heeft zoals `-la` voor het creëren van lijstvariabelen uit de regel - geweldig voor waarden gescheiden door komma's.

Aan de andere kant is `cat` eenvoudig. Het concateneert en toont de inhoud van bestanden. Het bestaat al in Unix sinds altijd (nou ja, exact sinds 1971).

Qua prestaties zijn directe lezingen over het algemeen sneller en oké voor kleinere bestanden. Maar als je een Moby Dick-formaat tekstbestand hebt, overweeg dan regel-voor-regel verwerking of gereedschappen zoals `sed`, `awk`, of zelfs `grep` als je specifieke regels aan het hengelen bent.

## Zie ook
- De [officiële Fish documentatie](https://fishshell.com/docs/current/index.html) voor een diepe duik in alles wat met Fish Shell te maken heeft.
- Een [Unix StackExchange-thread](https://unix.stackexchange.com/questions/tagged/fish) voor bredere gemeenschapsondersteuning en inzichten.
- Een tutorial over [het gebruik van awk in shell-scripting](https://www.gnu.org/software/gawk/manual/gawk.html) kan handig zijn als er complexere tekstverwerkingstaken opduiken.
