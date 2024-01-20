---
title:                "Att starta ett nytt projekt"
html_title:           "Arduino: Att starta ett nytt projekt"
simple_title:         "Att starta ett nytt projekt"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att starta ett nytt projekt är att ge sig i kast med en ny idé eller applikation från grunden. Programmerare gör detta för att skapa, lära, lösa problem eller bidra till öppen källkodsgemenskapen.

## Hur man gör:

Nu ska vi skapa en ny katalog med Fish Shell. Så här kan det se ut:

```Fish Shell
cd ~/
mkdir nytt_projekt
cd nytt_projekt
```

Därefter initialiserar vi en git tro med `git init`. Vi lägger till alla filer och commitar sedan med ett initialtaggande.

```Fish Shell
git init
touch README.md
git add .
git commit -m "Initial commit"
```

Vårt nya projekt är nu klart att utvecklas! Vad du lägger till kommer att vara beroende av projektet du skapar.

## Djupdykning

Historiskt sett är Shell-tolkar en grundläggande del av Unix-liknande system. Fish Shell skapades för att tillhandahålla en användarvänlig och effektiv kommandotolk.

Det finns alternativ till Fish Shell som Bash och Zsh. Men Fish Shell är känt för dess avancerade funktioner som syntax highlighting och auto-suggereringar.

Att starta ett nytt projekt kan variera beroende på dess tänkta användning. Implementationen kan inkludera att koppla till en databas, skapa en webbserver eller kanske både och.

## Se Även:

[Vad är Fish Shell?](https://fishshell.com)

[Grundläggande Git-kommandon](https://git-scm.com/book/sv/v2)

[Att starta ett öppet källkodsprojekt](https://opensource.guide/starting-a-project/)