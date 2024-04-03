---
date: 2024-01-20 18:03:25.691789-07:00
description: "Hur g\xF6r man: H\xE4r \xE4r ett grundl\xE4ggande exempel f\xF6r att\
  \ starta ett nytt projekt i Fish Shell."
lastmod: '2024-03-13T22:44:38.338782-06:00'
model: gpt-4-1106-preview
summary: "H\xE4r \xE4r ett grundl\xE4ggande exempel f\xF6r att starta ett nytt projekt\
  \ i Fish Shell."
title: "Att p\xE5b\xF6rja ett nytt projekt"
weight: 1
---

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
