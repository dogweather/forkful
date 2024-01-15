---
title:                "Utskrift av feilsøkingsutgang"
html_title:           "Fish Shell: Utskrift av feilsøkingsutgang"
simple_title:         "Utskrift av feilsøkingsutgang"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive debug-utdata er en nyttig teknikk for å feilsøke og forbedre koden din. Ved å skrive ut variabler og verdier under kjøring, kan du enkelt identifisere problemer og gjøre endringer for å forbedre funksjonaliteten.

## Slik gjør du det

```Fish Shell``` har innebygde kommandoer som lar deg skrive ut debug-utdata. For å skrive ut en variabel, bruk kommandoen ```echo``` og legg til variabelnavnet:

```
set variabelnavn "verdi"
echo $variabelnavn
```

Dette vil skrive ut verdien av variabelen i terminalen. Du kan også skrive ut tekststrenger og tall ved å omgi dem med anførselstegn:

```
echo "Dette er en tekststreng"
echo 123
```

Hvis du vil skrive ut flere variabler eller verdier samtidig, kan du bruke kommandoen ```printf```. Dette lar deg skrive ut en formatert streng med variabelverdier:

```
set navn "John"
set alder 25
printf "Navnet mitt er %s og jeg er %d år gammel" $navn $alder
```

Dette vil skrive ut "Navnet mitt er John og jeg er 25 år gammel" i terminalen.

## Dypdykk

Ved å bruke debug-utdata kan du enkelt se hvordan variabler endres under kjøring og identifisere eventuelle problemer. For å gjøre dette mer effektivt, kan du også inkludere en melding for hver debug-linje for å mer spesifikt identifisere hva som skrives ut.

Du kan også bruke enda mer avanserte metoder for å formatere og skrive ut debug-utdata, som å bruke ```status``` for å kontrollere resultatet av en kommando, eller å vise innholdet i en fil ved hjelp av kommandoen ```cat```. Bruk av riktig formatering og inkludering av nyttig informasjon kan gjøre debug-utdataene mer leselige og forståelige.

## Se også

- [Fish Shell offisiell dokumentasjon](https://fishshell.com/docs/current/index.html)
- [Debugging Strategies and Best Practices](https://www.codecademy.com/articles/fish-debugging-strategies)
- [Using printf in Fish Shell](https://medium.com/@ogratton/using-printf-in-fish-shell-to-output-formatted-strings-3ff2c50cf24c)