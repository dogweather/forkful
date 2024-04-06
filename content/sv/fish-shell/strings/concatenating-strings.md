---
date: 2024-01-20 17:34:49.963072-07:00
description: "S\xE5 h\xE4r g\xF6r du: Konkatenering i Fish shell \xE4r rakt p\xE5\
  \ sak \u2013 du placerar helt enkelt variablerna eller str\xE4ngarna intill varandra.\
  \ Historiskt sett har m\xE5nga\u2026"
lastmod: '2024-04-05T21:53:39.665136-06:00'
model: gpt-4-1106-preview
summary: "Konkatenering i Fish shell \xE4r rakt p\xE5 sak \u2013 du placerar helt\
  \ enkelt variablerna eller str\xE4ngarna intill varandra."
title: "Sammanslagning av str\xE4ngar"
weight: 3
---

## Så här gör du:
```Fish Shell
# Enkelt exempel på konkatenering
set hello "Hej "
set world "världen"
set greeting "$hello$world"
echo $greeting # Skriver ut "Hej världen"

# Med variabelsubstitution
set file_path "/min/mapp/"
set file_name "dokument.txt"
set full_path "$file_path$file_name"
echo $full_path # Skriver ut "/min/mapp/dokument.txt"
```

## Djupdykning
Konkatenering i Fish shell är rakt på sak – du placerar helt enkelt variablerna eller strängarna intill varandra. Historiskt sett har många shellskriptspråk, som Bash och Zsh, använt liknande syntax för konkatenering. Alternativen inkluderar att använda `echo` eller `printf` för att direkt sammanfoga värden. Intern arbetar Fish med strängar som variabler och tillåter direkt sammansättning utan behov av extra operatorer som man kan se i andra programmeringsspråk.

## Se även
- Fish Shell's officiella dokumentation om strängmanipulation: [https://fishshell.com/docs/current/index.html#expand](https://fishshell.com/docs/current/index.html#expand)
- Unix StackExchange, diskussioner kring strängbehandling i Fish: [https://unix.stackexchange.com/questions/tagged/fish](https://unix.stackexchange.com/questions/tagged/fish)
- Fish shell-tutorial om att hantera och använda variabler: [https://fishshell.com/docs/current/tutorial.html#tut_variables](https://fishshell.com/docs/current/tutorial.html#tut_variables)
