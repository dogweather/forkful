---
title:                "Ta bort citattecken från en sträng"
aliases:
- /sv/bash/removing-quotes-from-a-string/
date:                  2024-01-26T03:37:48.360979-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ta bort citattecken från en sträng"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ta bort citattecken från en sträng innebär att man avlägsnar de citationstecken som omsluter strängen. Programmerare vill ofta göra detta för att sanera indata, förbereda data för jämförelser eller följa ett specifikt dataformat när de interagerar med andra program eller system.

## Hur man gör:
Bash har flera sätt att ta bort citattecken från strängar. Här är några snabba exempel:

```Bash
#!/bin/bash

# Använder variabelsubstitution för att ta bort både enkla och dubbla citattecken
STRING="\"Hej, världen!\""
echo ${STRING//\"}

# Använder `tr` för att ta bort citattecken
STRING="'Hej, världen!'"
echo $STRING | tr -d "\'"

# Använder `sed` för att ta bort citattecken
STRING="\"Hej, världen!\""
echo $STRING | sed 's/"//g'
```

Exempel på utdata:

```
Hej, världen!
Hej, världen!
Hej, världen!
```

## Fördjupning
För länge sedan var Unix-kommandon som `tr` och `sed` de primära verktygen för textbehandling. De används fortfarande idag på grund av deras flexibilitet och kraft i att hantera texttransformationer som att ta bort citattecken. De är en grundpelare i verktygslådan för alla som skriptar i skal.

Bash har sedan dess utvecklats och variabelsubstitution lägger till ytterligare ett lager av enkelhet för småskaliga strängmanipulationer. Det sparar dig från att pipa ut till externa binärer, vilket gör dina skript lite mer effektiva.

Medan `tr` är utmärkt för att ta bort tecken, hanterar det inte mer komplexa mönster. `Sed`, å andra sidan, använder reguljära uttryck, så det är ibland overkill och kan vara långsammare för enkla operationer.

Att välja mellan dessa metoder beror på ditt specifika fall. Om du behöver ta bort en variation av citattecken och du redan befinner dig i kontexten av ett Bash-skript, är användningen av variabelsubstitution en självklarhet för dess enkelhet. Men om du transformerar textströmmar eller data över flera rader, är `tr` och `sed` dina go-to-kompisar.

## Se även:
- GNU Bash-manualen, särskilt avsnitten om Parameter Expansion och Shell Parameter Expansion: https://www.gnu.org/software/bash/manual/
- Manualen för kommandot `tr`: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- Översikten över strömredigeraren `sed`: https://www.gnu.org/software/sed/manual/sed.html
