---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:16.438387-07:00
description: 'Hoe te: Hier is hoe je een server netjes om gegevens vraagt met een
  ''alsjeblieft'' in de vorm van basisauthenticatie.'
lastmod: '2024-03-13T22:44:51.028793-06:00'
model: gpt-4-0125-preview
summary: Hier is hoe je een server netjes om gegevens vraagt met een 'alsjeblieft'
  in de vorm van basisauthenticatie.
title: Een HTTP-verzoek verzenden met basisauthenticatie
weight: 45
---

## Hoe te:
Hier is hoe je een server netjes om gegevens vraagt met een 'alsjeblieft' in de vorm van basisauthenticatie:

```PowerShell
# De inloggegevens voorbereiden
$user = 'JouwGebruikersnaam'
$pass = 'JouwWachtwoord'
$pair = "$($user):$($pass)"
$encodedCreds = [System.Convert]::ToBase64String([System.Text.Encoding]::ASCII.GetBytes($pair))

# De koppen instellen
$headers = @{
    Authorization = "Basic $encodedCreds"
}

# De URL waar je aanklopt
$url = 'https://api.voorbeeld.com/data'

# Nu, laten we de oproep doen
$response = Invoke-RestMethod -Uri $url -Method Get -Headers $headers

# De resultaten uitvoeren
$response
```

Een voorbeelduitvoer kan er als volgt uitzien, ervan uitgaande dat de respons in JSON-formaat is:

```json
{
    "name": "John Doe",
    "email": "john@voorbeeld.com"
}
```

## Diepere Duik
Basisauthenticatie is ouderwets en gaat terug naar de vroege dagen van het internet toen iedereen elkaar kende. Hoewel het nog steeds wordt gebruikt, is het op zichzelf niet superveilig - het is alsof je je geheime clubwachtwoord op een ansichtkaart verzendt. Tegenwoordig verzenden we het meestal over HTTPS om het te versleutelen, wat is alsof je die ansichtkaart in een gesloten doos stopt.

Alternatieven? Genoeg. Je hebt API-sleutels, OAuth, bearer tokens... de lijst gaat maar door. Elk komt met zijn eigen handdrukken en geheime woorden.

Wat de implementatie betreft, met PowerShell, converteer je je gebruikersnaam en wachtwoord naar een formaat dat het HTTP-protocol kan begrijpen - base64. Maar onthoud, base64 is geen encryptie; het is gewoon tekst die zich vermomt. Elke snuffelaar kan het onthullen, tenzij het over HTTPS wordt verzonden.

## Zie Ook
- [Documentatie van Invoke-RestMethod](https://docs.microsoft.com/nl-nl/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- [HTTP Basic Access Authentication op MDN](https://developer.mozilla.org/nl/docs/Web/HTTP/Authentication)
- [Begrip van Base64-Codering](https://nl.wikipedia.org/wiki/Base64)
- [Info over HTTPS Encryptie](https://nl.wikipedia.org/wiki/HTTPS)
