---
title:                "Eine HTTP-Anforderung senden"
html_title:           "Bash: Eine HTTP-Anforderung senden"
simple_title:         "Eine HTTP-Anforderung senden"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?

Ein HTTP-Request ist eine Anforderung an einen Webserver, um bestimmte Informationen zu erhalten. Programmierer verwenden diese häufig, um API-Daten zu holen, Webseiten zu scrapen, oder um mit anderen Diensten im Internet zu kommunizieren.

## So geht's:

```Fish Shell
# Installation von httpie
brew install httpie

# Senden einer GET-Anforderung
http GET beispiel.de

# Senden einer POST-Anforderung mit Daten
http POST beispiel.de name=Max
```

Die Ausgabe könnte so aussehen:

```Fish Shell
HTTP/1.1 200 OK
Content-Type: text/html; charset=UTF-8
```

## Vertiefende Infos

Historisch gesehen wurden HTTP-Requests meist mit Tools wie `curl` oder `wget` gesendet. Heute gibt es jedoch eine Vielzahl von alternativen Methoden, unter anderem Bibliotheken wie `requests` in Python oder `axios` in JavaScript.

"Httpie" ist hier das Tool der Wahl und eine gute Wahl für das Fisch-Shell, da es leicht zu bedienen ist und klare, farblich codierte Antworten liefert. Es ist jedoch wichtig, zu verstehen, dass HTTP-Anfragen auf niedrigerer Ebene erfolgen und bestimmte Details wie Header oder Body-Daten enthalten können, die die Anfrage beeinflussen.

## Siehe auch

Für mehr Details zu `httpie` besuchen Sie deren offizielle Dokumentation unter https://httpie.io/docs.
Für weiterführende Informationen zum HTTP-Protokoll und dessen Verwendung, besuchen Sie https://www.w3schools.com/tags/ref_httpmethods.asp