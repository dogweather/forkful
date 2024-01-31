---
title:                "Att påbörja ett nytt projekt"
date:                  2024-01-20T18:03:25.691789-07:00
model:                 gpt-4-1106-preview
simple_title:         "Att påbörja ett nytt projekt"

category:             "Fish Shell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Starta ett nytt projekt innebär att skapa en ren arbetsyta för att utveckla dina idéer. Programmerare gör detta för att organisera, förenkla och effektivisera sitt arbete.

## Hur gör man:
Här är ett grundläggande exempel för att starta ett nytt projekt i Fish Shell:

```Fish Shell
# Skapa en ny katalog för ditt projekt
mkdir mitt_nya_projekt

# Gå in i den nya katalogen
cd mitt_nya_projekt

# Initiere ett Git-repositorium om du använder versionskontroll
git init

# Skapa en grundläggande filstruktur
mkdir bin src doc
touch README.md
```

När kommandona körs ska det se ut så här:

```Shell
/Way/to/folder> mkdir mitt_nya_projekt
/Way/to/folder> cd mitt_nya_projekt
/mitt_nya_projekt> git init
Initialized empty Git repository in /Way/to/folder/mitt_nya_projekt/.git/
/mitt_nya_projekt> mkdir bin src doc
/mitt_nya_projekt> touch README.md
```

## Fördjupning
Fish Shell, eller "friendly interactive shell", är en relativt ny shell jämfört med bash och zsh. Den släpptes första gången 2005 med fokus på användarvänlighet och smarta funktioner såsom tydlig syntax, autoförslag och script som är lätta att läsa. Alternativ till Fish är Bash, Zsh, och PowerShell, bland andra. När du startar ett nytt projekt i Fish kan du dra nytta av dess unika funktioner, t.ex. dess universella variabler som stannar kvar över sessioner utan behov av att exporteras.

## Se även
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html) - Officiell dokumentation.
- [Fish Shell GitHub Repository](https://github.com/fish-shell/fish-shell) - Fish Shell kodbas och bidrag.
- [Git SCM](https://git-scm.com/) - Hemsida för Git, versionskontrollsystemet.
