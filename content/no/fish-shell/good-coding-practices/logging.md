---
title:                "Loggføring"
aliases: - /no/fish-shell/logging.md
date:                  2024-01-26T01:03:39.711175-07:00
model:                 gpt-4-1106-preview
simple_title:         "Loggføring"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/logging.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Logging er i grunn å notere ned hva applikasjonen din gjør—en slags dagbok, hvis du vil, men for kode. Programmerere gjør dette for å holde styr på de små detaljene, som statusendringer, systemhendelser og irriterende feil, og sikre at ingen ujevnheter går ubemerket forbi.

## Hvordan gjøre det:
I Fish kan logging være så enkelt som å omdirigere standard ut- og feilstrømmer til en fil. La oss lage en loggpost for vår skripts start- og slutttider.

```fish
function log_start
  echo (date "+%Y-%m-%d %H:%M:%S") " - Skriptet startet" >> min_app.log
end

function log_end
  echo (date "+%Y-%m-%d %H:%M:%S") " - Skriptet avsluttet" >> min_app.log
end

log_start
# ... din skripts oppgaver ...
log_end

cat min_app.log
```

Her er hva du ville se i `min_app.log`:

```
2023-04-01 10:35:47  - Skriptet startet
2023-04-01 10:36:02  - Skriptet avsluttet
```

For avansert logging kan du bruke funksjoner med parametere for loggnivå og meldinger:

```fish
function log_message --argument message
  switch "$argv[1]"
    case 'INFO' 'WARN' 'ERROR'
      set log_level $argv[1]
    case '*'
      set log_level 'DEBUG'
  end
  set log_msg (string join " " $argv[2..-1])
  echo (date "+%Y-%m-%d %H:%M:%S") "[$log_level]" $log_msg >> min_app.log
end

log_message INFO "Dette er en informativ melding."
log_message ERROR "Noe gikk galt!"
```

Eksempel `min_app.log` utskrift vil være:
```
2023-04-01 10:35:47 [INFO] Dette er en informativ melding.
2023-04-01 10:35:49 [ERROR] Noe gikk galt!
```

## Dypdykk
Historisk var logging i skript vanligvis gjort med en haug `echo`-statements, og mens dette sikkert fortsatt er et alternativ, kan implementering av mer komplekse systemer være en utfordring. Fish har ikke en innebygd loggingsmekanisme som noen andre shells eller programmeringsspråk har, så ofte må man lage sin egen.

Alternativer til Fish sin innebygde `echo`-kommando for logging inkluderer Unix-verktøy som `syslog` eller `logger`, som samhandler med systemloggdaemonen, og gir en mer integrert tilnærming til logging av systemomfattende hendelser.

Fish sin enkelhet gjør at du kan opprette funksjoner for å håndtere loggingsdetaljenivået, å sette forskjellige nivåer som du kan skru av eller på. Noen implementeringer kan til og med inkludere navnet på skriptet, linjenummer, og tidsstempel, som gjør det lettere å spore tilbake trinnene som førte til en hendelse.

## Se også
- Fish Shell dokumentasjonen om å skrive funksjoner: https://fishshell.com/docs/current/#syntax-function
- Grunnleggende Shell Skripting Tips: https://developer.ibm.com/tutorials/l-lpic1-103-4/
- Guide til Syslog Protokollen: https://tools.ietf.org/html/rfc5424
