---
title:                "Å bruke en feilsøker"
date:                  2024-01-26T03:47:46.011422-07:00
model:                 gpt-4-0125-preview
simple_title:         "Å bruke en feilsøker"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/using-a-debugger.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å bruke en feilsøker i Bash betyr å utnytte verktøy for å teste og finne problemer i skriptene dine, som å fange feil som får koden din til å krasje eller i smug får den til å oppføre seg feil. Programmerere gjør dette fordi det er mye smartere å fange opp feil før de skaper kaos i et live miljø.

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