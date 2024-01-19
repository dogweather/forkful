---
title:                "Sammenligner to datoer"
html_title:           "Clojure: Sammenligner to datoer"
simple_title:         "Sammenligner to datoer"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Sammenligne to datoer innebærer rett og slett å sjekke hvilken dato som kommer før, om de er samme dato, eller etter. Dette er nyttig i programmering for å håndtere hendelser og logikk som oppstår ut fra sekvensen av tid og dato.

## Hvordan Gjøre Dette:

Her er et eksempel på hvordan du sammenligner to datoer i Fish Shell.

```Fish Shell
set dato1 (date -u -d '2022-06-01' +%s)
set dato2 (date -u -d '2022-08-01' +%s)

if test $dato1 -lt $dato2
    echo "Dato1 er før Dato2."
else if test $dato1 -eq $dato2
    echo "Dato1 er lik Dato2."
else
    echo "Dato1 er etter Dato2."
end
```
I dette eksemplet blir dato1 og dato2 konvertert til antall sekunder siden "Unix Epoch" (1970-01-01 00:00:00 UTC) ved å bruke `date`-kommandoen med `-u`-parameteren for å bruke UTC-tid og `-d`-parameteren for å angi datoen.

## Dypdykk:

Sammenligning av datoer som vi ser det i dag, var ikke alltid så enkelt. Tidligere versjoner av shell-scripting språk leverte ikke innebygde kommandoer for dato-håndtering. Dette ledet til mange kreative (og ofte komplekse) løsninger. Heldigvis gir moderne shell språk som Fish et bredt spekter av innebygde kommandoer, inkludert `date` og `test`, som i kombinasjon gjør det mulig å sammenligne datoer enkelt.

Når det gjelder alternativer, kan du også bruke `date -r` for å konvertere sekunder siden Unix Epoch til en lesbar dato, eller bruke `date +%s` for å få dagens dato i sekunder siden Unix Epoch.

Implementeringsdetaljer inkluderer hvordan `date`-kommandoen tolker datoer, and på serverens tidsinnstilling, som kan påvirke hvordan datoer tolkes. I tillegg er det viktig å huske på at Fish benytter seg av 64-bits heltall for å representere datoer, så det er ingen fare for Year 2038 problemet.

## Se Også:

Mer om sammenligning av datoer og tid i Fish Shell kan du lese på Fish’s offisielle dokumentasjonsside: https://fishshell.com/docs/current/

For historisk sammenheng og en dypere forståelse av Unix Epoch, kan du sjekke ut denne kilden: https://www.unixtutorial.org/unix-timestamp