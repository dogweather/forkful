---
title:                "Att göra en sträng versal"
date:                  2024-01-19
html_title:           "Bash: Att göra en sträng versal"
simple_title:         "Att göra en sträng versal"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kapitalisera en sträng innebär att omvandla dess första bokstav i varje ord till versal, eller hela strängen till versaler. Programmerare gör detta för att standardisera textdata, förbättra läsbarheten eller följa konventioner i brukargränssnitt.

## Hur man gör:
```Fish Shell
# För att göra första bokstaven i varje ord till versal:
echo "hej världen" | string capitalize

# Output
Hej Världen

# För att göra hela strängen till versaler:
echo "hej världen" | string upper

# Output
HEJ VÄRLDEN
```

## Djupdykning
Kapitalisering i Fish Shell utförs enkelt med inbyggda kommandon som `string capitalize` och `string upper`. Historiskt sett har shell-skripting använt olika externa verktyg som `awk` eller `tr` för textmanipulation, men Fish erbjuder en inbyggd syntax som gör det enklare.

Alternativ till Fishs inbyggda funktioner inkluderar att använda `awk '{print toupper($0)}'` för att konvertera till versaler eller att implementera en skräddarsydd funktion i Fish som hanterar specifika kapitaliseringsbehov.

När det gäller genomförandet, använder `string capitalize` Unicode för att korrekt identifiera ordgränser och hantera versaliseringen även för icke-engelska språk. Detta är ett exempel på Fishs förmåga att hantera modern och internationell textbehandling.

## Se även
- Fish Shell officiella dokumentation om strängar: [https://fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
- Unicode standard för textbehandling: [https://home.unicode.org](https://home.unicode.org)
- AWK handbok för textbehandling: [https://www.gnu.org/software/gawk/manual/gawk.html](https://www.gnu.org/software/gawk/manual/gawk.html)
