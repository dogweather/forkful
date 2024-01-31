---
title:                "Skriva till standardfel"
date:                  2024-01-19
simple_title:         "Skriva till standardfel"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skriva till standardfel (`stderr`) är ett sätt att skicka felmeddelanden och loggar separat från vanlig utdata (`stdout`). Programmerare gör detta för att diagnostisera problem och för att hålla fel och huvudinformation åtskilda.

## Så här gör du:
För att skriva till `stderr` i Bash, använd `>&2`. Här är exemplet:

```Bash
echo "Detta är en vanlig utskrift."
echo "Det här är ett felmeddelande!" >&2
```
Sample output kan se ut så här:
```
Detta är en vanlig utskrift.
Det här är ett felmeddelande!
```
Observera att även om felmeddelandet visas på skärmen, är det skickat till en annan ström (`stderr`).

## Djupdykning:
Historiskt sett skapades två separata strömmar för att hjälpa till med processkommunikation och felhantering. Det gör att utdata kan pipas eller omdirigeras separat från felmeddelanden. Andra alternativ inkluderar att skriva till en loggfil eller använda verktyg som `logger` för att hantera fel. I implementeringsdetaljer, `stderr` är vanligtvis filbeskrivare 2 och kan omdirigeras med `2>`, `2>>`, eller i kombination med `stdout` med `&>`.

## Se även:
- Bash manual: https://www.gnu.org/software/bash/manual/
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/
- Stack Overflow har diskussioner och exempel på hur man hanterar `stderr` och `stdout`: https://stackoverflow.com/
