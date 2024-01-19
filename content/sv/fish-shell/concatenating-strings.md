---
title:                "Sammanslagning av strängar"
html_title:           "C++: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att konkatenera strängar innebär att man sätter ihop två eller flera strängar för att bilda en. Programmerare gör detta för att manipulera data, skapa dynamiska uttryck eller helt enkelt för att förenkla utskrift i konsol.

## Hur gör man:

I Fish Shell är det rätt så enkelt att konkatenera strängar. Du behöver bara placera dem bredvid varandra. Här är ett exempel:

```Fish Shell
set str1 "Hej"
set str2 "Världen"
echo $str1 $str2
```

Som kommer att ge följande utskrift:
```
Hej Världen
```

## På djupet

Historiskt sett, har konkatenering av strängar varit en grundläggande operation i de flesta programmeringsspråk. Det är inget nytt eller specifikt för Fish Shell. När det gäller alternativ, kan du i Fish Shell också använda "string join" för att sätta ihop strängar med en viss separator:

```Fish Shell
set str1 "Hej"
set str2 "Världen"
echo (string join " " $str1 $str2)
```

Vid implementeringen av strängkonkatenering, översätter Fish Shell bara direkt till systemets egna strängoperationer. Faktum är att alla strängar i Fish tekniskt sett är listor under huven, så "concatenating strings" är egentligen just en sammanfogning av dessa listor.

## Se även

- [Fish Shell Dokumentation](https://fishshell.com/docs/current/)
- [Fish Shell String Command](https://fishshell.com/docs/current/cmds/string.html)
- [Friendly Interactive Shell (Fish)](https://en.wikipedia.org/wiki/Friendly_interactive_shell)
- [String Join Operation In Different Programming Languages](https://www.geeksforgeeks.org/join-function-python/)