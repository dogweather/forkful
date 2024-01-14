---
title:    "Bash: Sjekke om en mappe eksisterer"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Hvorfor

Du har kanskje lurt på hvorfor noen ganger må man sjekke om en mappe eksisterer før man iverksetter visse handlinger i Bash-programmering. Dette er fordi å forsøke å utføre kommandoer i en ikke-eksisterende mappe kan føre til uønskede konsekvenser eller feil i koden din. Så det er alltid en god ide å sjekke om en mappe eksisterer før du begynner å kjøre Bash-skriptet ditt.

## Hvordan

For å sjekke om en mappe eksisterer, kan du bruke kommandoen `[-d <mappe>]` i if-setningen din. Her er et eksempel på hvordan dette kan se ut:

```Bash
if [ -d "$HOME/dokumenter" ]; then
  echo "Mappen finnes"
else
  echo "Mappen finnes ikke"
fi
```

I dette eksemplet sjekker vi om mappen "dokumenter" eksisterer i hjemmemappen vår (representert av variabelen `$HOME`). Hvis mappen eksisterer, vil utskriften være "Mappen finnes", hvis ikke vil utskriften være "Mappen finnes ikke". Du kan endre mappen du vil sjekke ved å erstatte `$HOME/dokumenter` med den ønskede mappen.

## Dypdykk

Det finnes også andre måter å sjekke om en mappe eksisterer på, som for eksempel å bruke kommandoen `test -d <mappe>` eller et logisk og-operator `&&`. Det viktigste å huske på er å bruke den `[-d]` kommandoen i if-setningen din, slik at den kan håndtere mappe-eksistens-sjekken på riktig måte.

## Se også

- [Shell Scripting Tutorial](https://www.shellscript.sh)
- [Bash scripting for beginners](https://www.linuxfordevices.com/tutorials/bash/bash-scripting-for-beginners)
- [Bash IF statement](https://www.tldp.org/LDP/Bash-Beginners-Guide/html/sect_07_01.html)