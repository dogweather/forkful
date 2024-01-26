---
title:                "Feilhåndtering"
date:                  2024-01-26T00:50:13.161501-07:00
model:                 gpt-4-1106-preview
simple_title:         "Feilhåndtering"
programming_language: "Bash"
category:             "Bash"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/handling-errors.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Håndtering av feil i Bash-skripting handler om å forutse hvor ting kan gå galt og håndtere det på en god måte. Hvorfor? Vel, det holder skriptet ditt robust og sparer brukere for hodebry når ting ikke fungerer som forventet.

## Hvordan gjøre:

```Bash
#!/bin/bash

# Omdirigering av stderr til en fil
grep "noe" fil.txt 2> feil.logg

# Feilhåndtering med exit-statuser
if ! grep "noe" fil.txt; then
    echo "Oops, noe gikk galt med søket etter 'noe'."
    exit 1
fi

# Bruk av en felle for å rydde opp før man avslutter på grunn av feil
cleanup() {
  echo "Rydder opp midlertidige filer..."
  rm temp_*
}

trap cleanup ERR

# med vilje feil: filen eksisterer ikke
cat temp_fil.txt
```

Eksempel på utdata når en feil forekommer:

```
Rydder opp midlertidige filer...
cat: temp_fil.txt: Ingen slik fil eller katalog
```

## Dypdykk

Feilhåndtering i Bash-skripting går tilbake til opprinnelsen til Unix-skallet, hvor robuste og pålitelige skript var (og er) avgjørende for systemadministrasjon og automatisering. Tradisjonelt er feil i Bash håndtert ved å sjekke utgangsstatus for en kommando, som etter konvensjon returnerer 0 for suksess og en ikke-null verdi for feil.

Bash introduserte kommandoen `trap` som en innebygd funksjon, som gir brukerne mulighet til å spesifisere kommandoer som skal kjøres ved forskjellige signaler eller skriptavslutninger. Dette er nyttig for opprydningsoppgaver eller som en siste utvei for feilhåndteringsmekanisme.

Det er også kommandoen `set`, som kan endre oppførselen til Bash ved feil. For eksempel vil `set -e` føre til at et skript avslutter umiddelbart hvis en kommando stopper med en ikke-null status, en måte å feile raskt på og unngå feil som eskalerer.

Alternativer til Bash sin innebygde feilhåndtering inkluderer eksplisitt sjekk for eksistensen av filer, bruk av kommandosubstitusjon, eller til og med å skrive dine egne funksjoner for å håndtere feil mer detaljert.

Selv om streng feilhåndtering noen ganger kan føles overflødig for små skript, er det en praksis som kan spare mye tid på feilsøking og forhindre uventet oppførsel både for deg og brukerne.

## Se Også

- Bash-manualen om skallparametere: https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameters
- Avansert Bash-skriptingguide seksjon om feilhåndtering: https://www.tldp.org/LDP/abs/html/exit-status.html
- En grundig guide til `trap`: https://mywiki.wooledge.org/SignalTrap

Husk, skripting er en kunstform, og hvordan du håndterer glippene og snubletrådene kan gjøre mesterverket ditt mer motstandsdyktig. Happy scripting!