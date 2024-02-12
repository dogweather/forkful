---
title:                "Debug-output afdrukken"
aliases: - /nl/powershell/printing-debug-output.md
date:                  2024-01-28T22:05:06.976620-07:00
model:                 gpt-4-0125-preview
simple_title:         "Debug-output afdrukken"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/powershell/printing-debug-output.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Debugoutput afdrukken is als een gesprek voeren met je code. Het gaat over het invoegen van printfuncties om te tonen wat er onder de motorkap van je programma gebeurt. Programmeurs doen dit om variabelen, de uitvoeringsstroom te controleren, en te begrijpen waarom iets mis kan gaan.

## Hoe:

Laten we het simpel houden en daadwerkelijk iets doen. We laten de waarde van een variabele zien, hoe een lus vordert, en vangen de lastige fout op die kan verschijnen.

```PowerShell
# De waarde van een variabele tonen
$name = "PowerShell Guru"
Write-Host "De waarde van naam is: $name"

# De voortgang van een lus monitoren
for ($i = 0; $i -lt 5; $i++) {
    Write-Host "We zijn bij lus nummer: $i"
}

# Een fout opvangen en afdrukken
try {
    Get-Item "C:\NonExistent\File.txt" -ErrorAction Stop
} catch {
    Write-Host "Oeps: $_"
}
```

Voorbeelduitvoer:

```
De waarde van naam is: PowerShell Guru
We zijn bij lus nummer: 0
We zijn bij lus nummer: 1
We zijn bij lus nummer: 2
We zijn bij lus nummer: 3
We zijn bij lus nummer: 4
Oeps: Kan pad 'C:\NonExistent\File.txt' niet vinden omdat het niet bestaat.
```

## Diepere Duik

Terug in de oude dagen van het computergebruik betekende debuggen vaak letterlijke fysieke insecten die met de hardware knoeiden. We zijn sindsdien een lange weg gekomen, waarbij nu de term "bug" wordt gebruikt voor codeproblemen, en "debugging" voor het oplossen ervan.

De `Write-Host` cmdlet is de PowerShell vriend voor het afdrukken op het scherm, wat prima is voor basis scripts. Maar er zijn coolere manieren om dit te doen: `Write-Verbose`, `Write-Debug`, `Write-Output`, en `Write-Information` zijn als verschillende smaken van output voor diverse gebruikssituaties. Ze bieden je gecontroleerde spraakzaamheid, wat geweldig is als je je script moet dempen of dingen moet loggen zonder de console te spammen.

Wat implementatie betreft, is de foutafhandeling van PowerShell bijzonder chique. Je kunt verschillende soorten uitzonderingen vangen met `try`, `catch`, `finally` blokken en beslissen hoe te reageren. Het is als een kies-je-eigen-avontuur voor foutenbeheer.

## Zie Ook

- [Over Try, Catch, Finally](https://docs.microsoft.com/en-us/powershell/scripting/learn/deep-dives/everything-about-exceptions?view=powershell-7.1)
