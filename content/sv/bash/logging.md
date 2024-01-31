---
title:                "Loggning"
date:                  2024-01-26T01:00:01.961980-07:00
model:                 gpt-4-1106-preview
simple_title:         "Loggning"

category:             "Bash"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/logging.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Loggning är praktiken att spela in händelser, fel och annan viktig information från de körande processerna i ett program till en fil eller en utströmning. Programmerare gör detta för att spåra sina applikationers beteende, avlusa problem och bibehålla en historisk förteckning över operationer som kan hjälpa till med framtida felsökning.

## Hur man gör:

I Bash kan loggning vara så enkelt som att omdirigera eller lägga till utskrift i en fil. Här är ett grundläggande exempel:

```Bash
echo "Startar scriptet..." >> script.log
# Dina scriptkommandon här
echo "Scriptet slutfördes den $(date)" >> script.log
```

För något mer avancerat kan du integrera syslog för systemövergripande loggning:

```Bash
logger "Anpassat meddelande från mitt script"
```

`logger` sänder ett loggmeddelande till syslog-tjänsten, som sedan hanterar det enligt systemets syslog-konfiguration.

Exempel på utdata som fångats i `script.log`:

```Bash
Startar scriptet...
Scriptet slutfördes den Tue Mar 23 09:26:35 PDT 2021
```

## Fördjupning

Historiskt sett, i Unix-lika system, har loggning underlättats av syslog-tjänsten, vilket möjliggör för olika applikationer och delar av systemet att centralt logga meddelanden. Detta möjliggör implementeringen av en standardiserad loggningsmekanism genom hela systemet.

När det kommer till alternativ, kan vissa överväga att använda `syslog-ng` eller `rsyslog` för mer avancerade loggningsfunktioner, eller skriva loggar till en tidsbaserad databas för analytiska ändamål. För applikationer med högre komplexitetsnivåer, kan användningen av ett dedikerat loggningsbibliotek eller en loggningsapplikation som Log4j (i Java-ekosystemet) eller Monolog (i PHP), vilket kan tillhandahålla strukturerade och konfigurerbara loggningsalternativ, vara meningsfullt även för ett script-språk som Bash.

Hur du implementerar loggning beror i stor utsträckning på din applikations krav. Om du bara behöver enkel utdata för att följa scriptets framsteg, är det lätt och bekvämt att lägga till meddelanden i en fil. Men för en mer skalbar och robust loggning, vill du integrera med ett loggsystem som stöder funktioner som loggrotation, loggnivåer och fjärrloggning.

## Se även

- `man`-sidorna för `logger`- och `syslog`-funktionerna är alltid din vän, prova `man logger` eller `man syslog`.
- För en djupgående titt på systemloggning, överväg att läsa dokumentationen för `rsyslog` och `syslog-ng`.
- För att ta reda på mer om den historiska kontexten och principerna bakom loggning i Unix-lika system, tillhandahåller `Syslog`-protokollet dokumenterat i RFC 5424 omfattande information.
