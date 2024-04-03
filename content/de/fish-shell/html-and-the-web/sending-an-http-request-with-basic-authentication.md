---
date: 2024-01-20 18:01:29.285556-07:00
description: "Ein HTTP-Request mit Basisauthentifizierung beinhaltet das Senden von\
  \ Benutzername und Passwort in kodierter Form zum Server, um Zugriff zu erhalten.\u2026"
lastmod: '2024-03-13T22:44:54.309134-06:00'
model: gpt-4-1106-preview
summary: Ein HTTP-Request mit Basisauthentifizierung beinhaltet das Senden von Benutzername
  und Passwort in kodierter Form zum Server, um Zugriff zu erhalten.
title: HTTP-Anfragen mit Basisauthentifizierung senden
weight: 45
---

## Was & Warum?

Ein HTTP-Request mit Basisauthentifizierung beinhaltet das Senden von Benutzername und Passwort in kodierter Form zum Server, um Zugriff zu erhalten. Programmierer nutzen dies, um geschützte Ressourcen automatisch abzufragen oder Aktionen auf Webdiensten auszuführen.

## So geht's:

```Fish Shell
# Benutzername und Passwort setzen
set USER "deinUsername"
set PASS "deinPasswort"

# Base64-Kodierung für Authentifizierung erstellen
set AUTH (echo -n "$USER:$PASS" | base64)

# HTTP-Request mit Authorization-Header senden
curl -H "Authorization: Basic $AUTH" https://deine-seite.de/pfad

# Beispiel-Ausgabe
# Zugriff gewährt oder HTTP-Response-Status, z.B.: HTTP/1.1 200 OK
```

## Deep Dive:

Historisch gesehen ist die Basisauthentifizierung eine der ersten Methoden, um Zugriffsbeschränkungen für Web-Ressourcen zu implementieren. Sie ist einfach aber nicht die sicherste, da die Zugangsdaten leicht entschlüsselbar sind, wenn sie abgefangen werden. Aus diesem Grund sollte sie nur über HTTPS verwendet werden. Alternativen wie OAuth bieten mehr Sicherheit und werden oft für moderne Anwendungen genutzt. Bei der Basisauthentifizierung werden Benutzername und Passwort mit Base64 kodiert und als 'Authorization'-Header im HTTP-Request gesendet, nicht im Request-Body oder in der URL, damit es nicht direkt lesbar ist.

## Siehe auch:

- cURL Dokumentation: https://curl.se/docs/manpage.html
- Fish Shell Dokumentation: https://fishshell.com/docs/current/index.html
- HTTP-Authentifizierung auf MDN: https://developer.mozilla.org/de/docs/Web/HTTP/Authentication
- Base64 Kodierung: https://de.wikipedia.org/wiki/Base64
