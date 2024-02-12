---
title:                "Fouten afhandelen"
aliases:
- /nl/powershell/handling-errors/
date:                  2024-01-28T22:01:57.704372-07:00
model:                 gpt-4-0125-preview
simple_title:         "Fouten afhandelen"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/powershell/handling-errors.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Fouten afhandelen in PowerShell betekent het voorspellen van ongelukken en deze soepel beheren. Programmeurs doen dit om crashes te voorkomen en gebruikers van nuttige feedback te voorzien.

## Hoe te:
```PowerShell
# Basis Try-Catch om uitzonderingen te behandelen
try {
    # Code die een fout kan veroorzaken
    $resultaat = 1 / 0
} catch {
    # Wat te doen als er een fout optrad
    Write-Host "Oeps, er is een fout opgetreden: $_"
}

# Een aangepast foutbericht uitgeven
try {
    Get-Item "nietbestaandbestand.txt" -ErrorAction Stop
} catch {
    Write-Host "Het bestand kon niet worden gevonden."
}

# De $Error variabele gebruiken om de laatste fout te inspecteren
```
## Diepere Duik
PowerShell heeft een lange weg afgelegd sinds het begin als Monad. Foutafhandeling werd mettertijd robuuster en biedt nu mogelijkheden vergelijkbaar met andere programmeertalen. De `try-catch-finally` syntax is zo'n kruisbestuiving van talen zoals C#. Voordat dit beschikbaar was, verlieten scripters zich sterk op het controleren van voorwaarden en gebruikten de `$Error` automatische variabele.

PowerShell heeft ook twee hoofdtypen van fouten: terminerende en niet-terminerende. Terminerende fouten zullen het script stoppen tenzij gevangen in een `try-catch` blok, terwijl niet-terminerende dat niet zullen doen tenzij je `-ErrorAction Stop` specificeert. Dit onderscheid is cruciaal omdat het fijne controle over foutafhandeling toestaat, beslissend of een fout echt de stopzetting van het hele script rechtvaardigt of simpelweg gelogd en genegeerd kan worden.

PowerShell's foutafhandeling staat ook toe voor een `finally` blok, dat loopt maakt niet uit wat - of er nu een fout optrad of niet. Het is geweldig voor opruimtaken.

Wanneer je diep in de scripting loopgraven zit, kun je ook specifieke uitzonderingstypen afhandelen, wat je nog fijnere controle geeft.

Als alternatief is er de oude school `-ErrorVariable` parameter om fouten te vangen zonder een uitzondering te gooien. En de `$?` variabele vertelt je of de laatste operatie succesvol was. Ze zijn handige tools, al zijn ze iets minder netjes dan een solide `try-catch`.

## Zie Ook
- [about_Try_Catch_Finally](https://docs.microsoft.com/nl-nl/powershell/module/microsoft.powershell.core/about/about_try_catch_finally?view=powershell-7.2)
