---
title:                "Loggning"
aliases:
- /sv/fish-shell/logging.md
date:                  2024-01-26T01:04:27.357626-07:00
model:                 gpt-4-1106-preview
simple_title:         "Loggning"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/logging.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att logga är i grund och botten att skriva ned vad din applikation håller på med – som en dagbok, om du så vill, men för kod. Programmerare gör det för att hålla koll på nitty-gritty, såsom tillståndsändringar, systemhändelser och irriterande buggar, och för att säkerställa att inga problem glider förbi obemärkta.

## Hur man gör:
I Fish kan loggning vara så enkelt som att omdirigera standard utdatan och felströmmar till en fil. Låt oss skapa en loggpost för vår scripts start- och sluttider.

```fish
function log_start
  echo (date "+%Y-%m-%d %H:%M:%S") " - Skript startade" >> my_app.log
end

function log_end
  echo (date "+%Y-%m-%d %H:%M:%S") " - Skript avslutades" >> my_app.log
end

log_start
# ... ditt skripts uppgifter ...
log_end

cat my_app.log
```

Så här skulle du se i `my_app.log`:

```
2023-04-01 10:35:47  - Skript startade
2023-04-01 10:36:02  - Skript avslutades
```

För avancerad loggning kan du använda funktioner med parametrar för loggnivå och meddelanden:

```fish
function log_message --argument message
  switch "$argv[1]"
    case 'INFO' 'WARN' 'ERROR'
      set log_level $argv[1]
    case '*'
      set log_level 'DEBUG'
  end
  set log_msg (string join " " $argv[2..-1])
  echo (date "+%Y-%m-%d %H:%M:%S") "[$log_level]" $log_msg >> my_app.log
end

log_message INFO "Detta är ett informativt meddelande."
log_message ERROR "Något gick fel!"
```

Exempel på `my_app.log`-utdata:
```
2023-04-01 10:35:47 [INFO] Detta är ett informativt meddelande.
2023-04-01 10:35:49 [ERROR] Något gick fel!
```

## Fördjupning
Historiskt sett gjordes loggning i skal-skript med en massa `echo`-påståenden, och även om detta verkligen fortfarande är ett alternativ, kan implementering av mer komplexa system vara en utmaning. Fish har inte en inbyggd loggningsmekanism som vissa andra skal eller programmeringsspråk gör, så du behöver ofta skapa din egen.

Alternativ till Fishs inbyggda `echo`-kommando för loggning inkluderar Unix-verktyg som `syslog` eller `logger`, vilka gränssnittar med systemloggdemonen, och tillhandahåller ett mer integrerat tillvägagångssätt för att logga systemomfattande händelser.

Fishs enkelhet låter dig skapa funktioner för att hantera detaljrikedomen i loggning, sätta olika nivåer som du kan aktivera eller avaktivera. Vissa genomföranden kan till och med inkludera namnet på skriptet, radnummer och tidsstämpel, vilket gör det enklare att spåra tillbaka de steg som ledde till en händelse.

## Se även
- The Fish Shell-dokumentation om att skriva funktioner: https://fishshell.com/docs/current/#syntax-function
- Grundläggande tips för skal-skriptning: https://developer.ibm.com/tutorials/l-lpic1-103-4/
- Guide till Syslog-protokollet: https://tools.ietf.org/html/rfc5424
