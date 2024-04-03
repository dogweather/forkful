---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:41.441612-07:00
description: "Bestanden ter plaatse bewerken met CLI one-liners in PowerShell gaat\
  \ over het direct wijzigen van bestanden vanaf de opdrachtregel, zonder de noodzaak\
  \ om\u2026"
lastmod: '2024-03-13T22:44:51.024962-06:00'
model: gpt-4-0125-preview
summary: Bestanden ter plaatse bewerken met CLI one-liners in PowerShell gaat over
  het direct wijzigen van bestanden vanaf de opdrachtregel, zonder de noodzaak om
  ze in een editor te openen.
title: Bestanden ter plekke bewerken met CLI one-liners
weight: 32
---

## Hoe te:


### Tekst vervangen in een enkel bestand
Laten we beginnen met een eenvoudige taak: je wilt alle instanties van "oldtext" vervangen door "newtext" in een bestand genaamd example.txt. Zo zou je dat doen:

```PowerShell
(Get-Content example.txt) -replace 'oldtext', 'newtext' | Set-Content example.txt
```

Deze one-liner leest de inhoud, voert de vervanging uit en schrijft de inhoud terug naar het originele bestand.

### Meerdere bestanden bewerken
Wat als je dezelfde wijziging op meerdere bestanden moet toepassen? Hier is een aanpak met behulp van een lus:

```PowerShell
Get-ChildItem *.txt | ForEach-Object {
  (Get-Content $_) -replace 'oldtext', 'newtext' | Set-Content $_
}
```

Dit fragment vindt alle `.txt` bestanden in de huidige directory en vervangt "oldtext" met "newtext" in elk ervan.

### Inhoud toevoegen aan het begin of einde van bestanden
Het toevoegen van inhoud kan ook worden gestroomlijnd:

```PowerShell
# Toevoegen aan het begin
"New first line`n" + (Get-Content example.txt) | Set-Content example.txt

# Toevoegen aan het einde
(Get-Content example.txt) + "`nNew last line" | Set-Content example.txt
```

Hier, voegen we simpelweg de nieuwe inhoud toe vóór of na de bestaande inhoud en slaan het weer op.

## Diepere duik
Historisch gezien wordt het ter plaatse bewerken vaker geassocieerd met Unix-tools zoals `sed` en `awk`. PowerShell, als een meer recente nieuwkomer, bevat geen specifieke functie voor ter-plaatse-bewerking standaard. Dit komt deels door de ontwerpfilosofie, die de nadruk legt op het belang van objecten boven tekststromen, in tegenstelling tot Unix-tools die de meeste invoer als tekst behandelen.

Alternatieven voor PowerShell voor deze taak zijn het gebruik van traditionele Unix-tools beschikbaar op Windows via Cygwin of het Windows Subsysteem voor Linux (WSL). Deze tools bieden vaak een beknoptere syntaxis voor het ter plaatse bewerken vanwege hun tekstgerichte ontwerp.

Qua implementatie is het belangrijk om op te merken dat de aanpak van PowerShell het volledig inlezen van het bestand in het geheugen omvat, veranderingen aan te brengen, en het vervolgens terug te schrijven. Hoewel dit goed werkt voor bestanden van gemiddelde grootte, kan het inefficiënt worden voor zeer grote bestanden. In dergelijke gevallen zou men kunnen overwegen om `.NET`-methoden rechtstreeks te gebruiken of alternatieve tools te gebruiken die ontworpen zijn voor het streamen van grote hoeveelheden data.

Ondanks deze overwegingen maken de flexibiliteit en uitgebreide functieset van PowerShell het een onschatbare tool voor het direct manipuleren van bestanden vanaf de opdrachtregel, vooral voor degenen die al verankerd zijn in het Windows-ecosysteem of beheer van cross-platform omgevingen.
