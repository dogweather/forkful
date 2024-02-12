---
title:                "Skriving til standardfeil"
aliases:
- /no/bash/writing-to-standard-error.md
date:                  2024-02-03T19:32:27.762491-07:00
model:                 gpt-4-0125-preview
simple_title:         "Skriving til standardfeil"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive til standard feil (stderr) i Bash handler om å dirigere feilmeldinger eller viktig diagnostisk utdata separat fra standard utdata (stdout). Programmerere gjør dette for å sikre at feilmeldinger lett kan identifiseres, loggføres, eller til og med ignoreres, noe som bistår i feilsøkings- og loggføringsprosesser.

## Hvordan gjøre det:
I Bash bruker du `>&2` for å omdirigere utdata til stderr. Her er et grunnleggende eksempel:

```bash
echo "Dette er en normal melding"
echo "Dette er en feilmelding" >&2
```

Å kjøre dette skriptet vil vise begge meldingene på konsollen, men hvis du omdirigerer dem, kan du skille stdout fra stderr. For eksempel:

```bash
bash script.sh > output.txt 2> error.txt
```

`output.txt` vil inneholde `"Dette er en normal melding"`, mens `error.txt` vil fange opp `"Dette er en feilmelding"`.

For et praktisk bruksområde, vurder et skript som behandler filer og rapporterer en feil hvis en fil ikke eksisterer:

```bash
filename="example.txt"

if [ ! -f "$filename" ]; then
    echo "$filename finnes ikke!" >&2
    exit 1
else
    echo "Behandler $filename"
fi
```

Eksempel på utdata direkte i konsollen når `example.txt` ikke eksisterer:

```
example.txt finnes ikke!
```

Det finnes ingen direkte tredjepartsbiblioteker i Bash for håndtering av stderr, ettersom omdirigering er nativt støttet og generelt tilstrekkelig. Men for komplekse applikasjoner kan loggingsrammeverk eller eksterne loggingsverktøy som `syslog` eller `log4bash` inkorporeres for å håndtere både stdout og stderr mer effektivt.
