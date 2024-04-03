---
date: 2024-01-20 18:02:16.318571-07:00
description: 'Wie geht das: Hier ist ein einfaches Beispiel, wie man eine HTTP-Anfrage
  mit Basic Authentication in PowerShell macht.'
lastmod: '2024-03-13T22:44:54.103620-06:00'
model: gpt-4-1106-preview
summary: Hier ist ein einfaches Beispiel, wie man eine HTTP-Anfrage mit Basic Authentication
  in PowerShell macht.
title: HTTP-Anfragen mit Basisauthentifizierung senden
weight: 45
---

## Wie geht das:
Hier ist ein einfaches Beispiel, wie man eine HTTP-Anfrage mit Basic Authentication in PowerShell macht:

```PowerShell
# Benutzername und Passwort festlegen
$Benutzername = 'MeinBenutzer'
$Passwort = 'MeinPasswort'

# Base64-Encoder für Credentials verwenden
$Base64AuthInfo = [Convert]::ToBase64String([Text.Encoding]::ASCII.GetBytes(("$Benutzername:$Passwort")))

# HTTP-Request mit Headers erstellen
$response = Invoke-RestMethod -Uri 'http://meineapi.de/daten' -Method Get -Headers @{Authorization=("Basic {0}" -f $Base64AuthInfo)}

# Antwort ausgeben
$response
```

Sample Output:

```
{Name: "Beispiel", Wert: "42", Status: "Erfolg"}
```

## Tiefere Einblicke
Historisch gesehen ist Basic Authentication ein altes, aber bewährtes Authentifizierungsprotokoll im HTTP-Standard. Es ist einfach zu implementieren, allerdings ist es ohne HTTPS unsicher, weil die Credentials im Klartext übertragen werden. Für zusätzliche Sicherheit kann OAuth verwendet werden, was Token anstatt von Benutzername und Passwort benutzt. In PowerShell spezifisch ist `Invoke-RestMethod` die bevorzugte Methode für Webanfragen, da sie hocheffizient und einfach zu benutzen ist.

## Siehe auch
- Microsoft-Dokumentation zu `Invoke-RestMethod`: https://docs.microsoft.com/de-de/powershell/module/microsoft.powershell.utility/invoke-restmethod
- Basic Authentication-Überblick: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Informationen zu OAuth: https://oauth.net
