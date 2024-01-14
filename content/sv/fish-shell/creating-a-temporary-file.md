---
title:                "Fish Shell: Skapa en tillfällig fil"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför
Att skapa en tillfällig fil är en användbar teknik för att temporärt lagra data eller resultat i ett script eller en program. Det kan också användas för att skydda känslig data och undvika permanenta ändringar.

## Hur man gör det
Det är enkelt att skapa en temporär fil i Fish Shell. Här är ett exempel på hur du kan skapa en tillfällig fil som lagrar några rader med text:

```Fish Shell
set -l tmp_file (mktemp) # Skapar en tillfällig fil och tilldelar den till en variabel
echo "Detta är en temporär fil" >> $tmp_file # Lägger till text i filen
less $tmp_file # Visar innehållet i filen
```

### Output

`
Detta är en temporär fil
`

Som du kan se i exemplet använder vi kommandot `mktemp` för att skapa den temporära filen. Det är ett kommando som redan är inbyggt i Fish Shell, så det behöver du inte installera någonting extra.

## Djupdykning
För att förstå hur `mktemp` fungerar kan det vara hjälpsamt att veta att den skapar en temporär fil med ett unikt namn. Det unika namnet genereras av en slumpmässig sträng och tillägger några prefix för att följa konventionerna för temporära filer.

Om du vill skapa en temporär fil i en annan katalog kan du använda flaggan `-p` följt av den önskade sökvägen. Till exempel:

```Fish Shell
mktemp -p ~/Desktop # Skapar en temporär fil på skrivbordet
```

Det finns också möjlighet att skapa en temporär mapp istället för en fil genom att använda flaggan `-d`.

## Se också
* [Fish Shell dokumentation](https://fishshell.com/docs/current/index.html)
* [Unix/Linux kommandon för att hantera filer](https://www.tecmint.com/linux-file-operations-commands/)