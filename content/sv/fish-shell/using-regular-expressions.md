---
title:                "Använda reguljära uttryck"
date:                  2024-01-19
html_title:           "Bash: Använda reguljära uttryck"
simple_title:         "Använda reguljära uttryck"

category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Reguljära uttryck är mönstersökning på steroider. Programmerare använder det för att finna, ersätta och manövrera text snabbt och precist.

## Hur gör man:
```
# Exempel: Hitta alla filer som slutar med .txt
ls *.txt

# Byt ut "fisk" med "fågel" i samtliga filer med .md-ändelse
sed -i 's/fisk/fågel/g' *.md
```

Output:
```
1. dokument.txt
2. anteckningar.txt
```

## Djupdykning:
Reguljära uttryck (regex) härstammar från 1950-talets automatteori. Alternativ till Fish är bash, zsh och PowerShell. Fish implementerar regex via kommandon som `string match` och externa verktyg som `grep`.

## Se även:
- Fish Shell dokumentation: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- Regex grunder: [https://www.regular-expressions.info/](https://www.regular-expressions.info/)
- Sed och Grep guider: `man sed`, `man grep`
