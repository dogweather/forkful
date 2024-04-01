---
date: 2024-01-26 03:47:46.011422-07:00
description: "\xC5 bruke en feils\xF8ker i Bash betyr \xE5 utnytte verkt\xF8y for\
  \ \xE5 teste og finne problemer i skriptene dine, som \xE5 fange feil som f\xE5\
  r koden din til \xE5 krasje eller\u2026"
lastmod: '2024-03-13T22:44:40.979452-06:00'
model: gpt-4-0125-preview
summary: "\xC5 bruke en feils\xF8ker i Bash betyr \xE5 utnytte verkt\xF8y for \xE5\
  \ teste og finne problemer i skriptene dine, som \xE5 fange feil som f\xE5r koden\
  \ din til \xE5 krasje eller\u2026"
title: "\xC5 bruke en feils\xF8ker"
---

## Hvordan:
Bash kommer ikke med en innebygd feilsøker som noen andre språk, men du kan bruke innebygde kommandoer som `set -x` for å spore hva som skjer. Eller, for en oppgradering, er det `bashdb`, en ordentlig feilsøker for å gå gjennom koden din. Her er en titt:

```Bash
# Bruker set -x for feilsøking
set -x
echo "Start feilsøking"
my_var="Hallo, Feilsøkingsverden!"
echo $my_var
set +x

# Bruker bashdb
# Installer bashdb med pakkebehandleren din, f.eks., apt, yum, brew.
# Feilsøk et skript kalt my_script.sh:
bashdb my_script.sh
```

Output når man kjører med `set -x`:
```Bash
+ echo 'Start feilsøking'
Start feilsøking
+ my_var='Hallo, Feilsøkingsverden!'
+ echo 'Hallo, Feilsøkingsverden!'
Hallo, Feilsøkingsverden!
+ set +x
```

## Dypdykk
Historisk sett betød feilsøking av Bash-skript at man strødde koden sin med `echo`-utsagn. Men så kom `set -x`, som ga oss en titt inn i kjøretidsutførelsen uten manuelle utskrifter. Og for de som ønsker mer kontroll, dukket feilsøkeren `bashdb` opp, inspirert av gdb-feilsøkeren for C/C++.

Når det gjelder alternativer, utover `set`-kommandoene (`-x`, `-v`, `-e`), inkluderer andre alternativer å omdirigere utdata til en fil for analyse eller å bruke eksterne verktøy som ShellCheck for statisk analyse.

Når det kommer til implementering, er `set -x` enkelt; det er et nativt Bash-alternativ som skriver ut kommandoer og deres argumenter etter hvert som de utføres. `bashdb`, på den annen side, tillater steg-for-steg gjennomgang av kode, å sette brytepunkter, og å evaluere uttrykk - ting som gir deg en kampmulighet mot mer unnvikende feil.

## Se Også
- Bash Debugger Project: http://bashdb.sourceforge.net/
- "Pro Bash Programming" av Chris Johnson og Jayant Varma for avansert skripting.
- ShellCheck for statisk analyse: https://www.shellcheck.net/
