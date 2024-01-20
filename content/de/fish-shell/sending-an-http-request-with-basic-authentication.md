---
title:                "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
html_title:           "Bash: Eine HTTP-Anfrage mit Basisauthentifizierung senden"
simple_title:         "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Senden einer HTTP-Anfrage mit Basic-Authentifizierung ist ein elementarer Schritt, der verwendet wird, um einen Server dazu zu bringen, sensible Daten zu liefern, nachdem eine gültige Benutzername-Passwort-Kombination vorgelegt wurde. Programmierer machen dies, um die Privatsphäre und Sicherheit von Userdaten in Netzwerkanwendungen zu gewährleisten.

## Wie macht man das:

Im Fish Shell-Codeblock unten sehen Sie, wie man eine HTTP-Anfrage mit Basic-Authentifizierung sendet:

```Fish Shell
set benutzername 'deinUsername'
set passwort 'deinPasswort'
set basisauth (echo -n $benutzername:$passwort | base64)
curl -H "Authorization: Basic $basisauth" https://deine-website.de
```
Die Ausgabe wäre dann die Antwort des Servers auf Ihre Anfrage.

## Tiefere Einblicke

Die Methode des Sendens von HTTP-Anfragen mit Basic-Authentifizierung ist zwar alt, aber sehr effektiv. Sie wurde bereits in den 90er Jahren eingeführt, als das WWW noch in den Kinderschuhen steckte. 

Alternativen zu Basic Auth sind Digest-Authentifizierung, OAuth und vieles mehr, die je nach Use Case verwendet werden können.

Das Prinzip der Basic Authentifizierung ist recht simpel: Benutzername und Passwort werden durch einen Doppelpunkt getrennt und base64-kodiert. Der resultierende String wird dann im HTTP-Header mitgeliefert.

## Siehe auch

Schau dir folgende Ressourcen für weitere Informationen an:

1. Fish Shell Offizielle Dokumentation: https://fishshell.com/
2. RFC 7617 - The 'Basic' HTTP Authentication Scheme: https://tools.ietf.org/html/rfc7617
3. Stack Overflow - Verwenden Sie curl mit HTTP Basic-Authentifizierung auf der Fish Shell: https://stackoverflow.com/questions/18377526/curl-basic-http-authentication-on-the-fish-shell