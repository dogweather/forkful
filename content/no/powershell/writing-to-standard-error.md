---
title:                "Å skrive til standardfeil"
html_title:           "PowerShell: Å skrive til standardfeil"
simple_title:         "Å skrive til standardfeil"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor? 
Skriver du noensinne kode som ikke fungerer som forventet? Kanskje får du en feilmelding i PowerShell, men du har vanskeligheter med å forstå hva som skjedde. Det er her skriving til standardfeil kommer inn i bildet. Det er en måte for programmerere å gi viktige feilmeldinger og informasjon om kjøretidsfeil til brukeren.

## Hvordan: 
Her er et eksempel på hvordan du skriver til standardfeil i PowerShell:

```PowerShell
Write-Error "Det oppsto en feil ved kjøring av dette skriptet."
```

Dette vil skrive ut feilmeldingen til standardfeil, og det vil også returnere en feilkode på 1. Dette kan være nyttig når du kjører skript i en automatisert prosess og trenger en tydelig måte å indikere en feil på.

## Dykk dypere: 
Historisk sett ble det å skrive til standardfeil brukt i programmeringsspråket C som en måte å håndtere feil på. I stedet for å krasje eller stoppe når det oppsto en feil, kunne programmet fortsette å kjøre og gi informasjon om hva som skjedde. I dag er skriving til standardfeil en vanlig praksis i de fleste programmeringsspråk, inkludert PowerShell.

Andre alternativer for håndtering av feil inkluderer å skrive til standardutgang, loggføre data til en fil eller håndtere feilen i koden selv. Men skriving til standardfeil gir en enkel måte å gi beskjed til brukeren om at noe gikk galt.

## Se også: 
For mer informasjon om hvordan du bruker skriving til standardfeil i PowerShell, se Microsofts offisielle dokumentasjon: https://docs.microsoft.com/en-us/powershell/scripting/debugging/using-write-error

Hvis du ønsker å lære mer om feilhåndtering i PowerShell generelt, kan du sjekke ut denne artikkelen på nettstedet How-To Geek: https://www.howtogeek.com/177699/how-to-automate-powershell-for-log-file-management/