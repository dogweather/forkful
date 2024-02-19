---
aliases:
- /de/bash/sending-an-http-request-with-basic-authentication/
date: 2024-01-20 18:00:44.724934-07:00
description: "Beim Senden einer HTTP-Anfrage mit Basic Authentication werden Benutzername\
  \ und Passwort kodiert und als Teil der Anfrage \xFCbermittelt. Programmierer\u2026"
lastmod: 2024-02-18 23:09:05.054145
model: gpt-4-1106-preview
summary: "Beim Senden einer HTTP-Anfrage mit Basic Authentication werden Benutzername\
  \ und Passwort kodiert und als Teil der Anfrage \xFCbermittelt. Programmierer\u2026"
title: HTTP-Anfragen mit Basisauthentifizierung senden
---

{{< edit_this_page >}}

## Was & Warum?

Beim Senden einer HTTP-Anfrage mit Basic Authentication werden Benutzername und Passwort kodiert und als Teil der Anfrage übermittelt. Programmierer nutzen dies, um sichere Zugänge zu Webdiensten zu schaffen, die eine einfache Authentifizierung erfordern.

## So geht's:

```Bash
# Anfrage mit curl und Basic Authentication
USER='benutzername'
PASS='passwort'
URL='https://api.deinwebdienst.com/datensatz'

# Base64-Kodierung der Anmeldedaten
ENCODED_CREDENTIALS=$(echo -n "$USER:$PASS" | base64)

# Ausführen der Anfrage
curl -H "Authorization: Basic $ENCODED_CREDENTIALS" $URL
```

Beispieloutput:

```
{"status":"Erfolg","message":"Du bist eingeloggt!"}
```

## Tiefgang

Basic Authentication wird bereits seit den Anfängen des Webs für einfache Authentifizierungsprozesse verwendet. Zwar gilt diese Art der Authentifizierung heute nicht als die sicherste Methode, da die Anmeldedaten unverschlüsselt über das Netz versendet werden können, sie ermöglicht jedoch einen schnellen Einstieg und wird oft in internen Netzwerken oder Prototypen verwendet. Alternativen wie OAuth oder Token-Based Authentication bieten zusätzliche Sicherheitsfeatures, sind aber auch komplexer in der Umsetzung.

Die Base64-Kodierung dient nicht der Verschlüsselung, sondern lediglich der Kodierung für die Übertragung über HTTP. Wichtig ist hierbei, dass die HTTPS verwendet wird, um die Daten auf dem Transportweg zu verschlüsseln.

## Siehe auch:

- cURL Dokumentation zu HTTP-Authentifizierung: https://curl.se/docs/http-auth.html
- RFC 7617, The 'Basic' HTTP Authentication Scheme: https://tools.ietf.org/html/rfc7617
- Einführung in sichere Authentifizierungsmethoden: https://www.owasp.org/index.php/Authentication_Cheat_Sheet
