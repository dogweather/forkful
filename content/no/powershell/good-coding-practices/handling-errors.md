---
title:                "Feilhåndtering"
aliases:
- /no/powershell/handling-errors.md
date:                  2024-01-26T00:55:55.369307-07:00
model:                 gpt-4-1106-preview
simple_title:         "Feilhåndtering"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/handling-errors.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å håndtere feil i PowerShell betyr å forutse uhell og håndtere dem på en smidig måte. Programmerere gjør dette for å forhindre krasjer og gi brukere nyttig tilbakemelding.

## Hvordan:
```PowerShell
# Grunnleggende Try-Catch for å håndtere unntak
try {
    # Kode som kan utløse en feil
    $result = 1 / 0
} catch {
    # Hva gjøre hvis en feil skjedde
    Write-Host "Oi, det oppstod en feil: $_"
}

# Skrive ut en egendefinert feilmelding
try {
    Get-Item "nonexistentfile.txt" -ErrorAction Stop
} catch {
    Write-Host "Filen ble ikke funnet."
}

# Bruker $Error-variabelen for å inspisere den siste feilen
```
## Dypdykk
PowerShell har kommet langt siden starten som Monad. Feilhåndtering ble mer robust over tid, og tilbyr funksjoner lik de i andre programmeringsspråk. `try-catch-finally`-syntaksen er ett slikt eksempel på kryssbestøvning fra språk som C#. Før dette, var skriptere sterkt avhengig av å sjekke forhold og å bruke `$Error`-automatikkvariabelen.

PowerShell har også to hovedtyper av feil: avsluttende og ikke-avsluttende. Avsluttende feil vil stoppe skriptet med mindre de fanges opp i en `try-catch`-blokk, mens ikke-avsluttende feil ikke vil stoppe det med mindre du spesifiserer `-ErrorAction Stop`. Denne distinksjonen er avgjørende siden den gir fin kontroll over feilhåndteringen, og bestemmer om en feil virkelig rettferdiggjør å stoppe hele skriptet eller kan rett og slett logges og ignoreres.

PowerShell sin feilhåndtering tillater også en `finally`-blokk, som kjører uansett hva - enten en feil oppsto eller ikke. Den er fin for opprydningsoppgaver.

Når du er dypt inne i skriptenes skyttergraver, kan du også håndtere spesifikke unntakstyper, noe som gir deg enda finere kontroll.

Alternativt har du den gamle skolen `-ErrorVariable` parameter for å fange opp feil uten å kaste et unntak. Og `$?`-variabelen forteller deg om den siste operasjonen var vellykket. De er nyttige verktøy, selv om de kan være litt mindre ryddige enn en solid `try-catch`.

## Se Også
- [about_Try_Catch_Finally](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_try_catch_finally?view=powershell-7.2)
