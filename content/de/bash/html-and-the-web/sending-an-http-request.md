---
date: 2024-01-20 17:59:02.407574-07:00
description: "HTTP-Anfragen (Hypertext Transfer Protocol) sind die Basis des Webs.\
  \ Programmierer nutzen sie, um mit Webservern zu kommunizieren \u2013 Daten abfragen\
  \ oder\u2026"
lastmod: '2024-02-25T18:49:51.108739-07:00'
model: gpt-4-1106-preview
summary: "HTTP-Anfragen (Hypertext Transfer Protocol) sind die Basis des Webs. Programmierer\
  \ nutzen sie, um mit Webservern zu kommunizieren \u2013 Daten abfragen oder\u2026"
title: Einen HTTP-Request senden
---

{{< edit_this_page >}}

## Was & Warum?
HTTP-Anfragen (Hypertext Transfer Protocol) sind die Basis des Webs. Programmierer nutzen sie, um mit Webservern zu kommunizieren – Daten abfragen oder senden.

## How to:
### Curl-Befehl
```Bash
curl https://api.example.com/data
```
Output:
```
{"name":"Beispiel","type":"JSON data"}
```

### Mit HTTP-Methoden
```Bash
# GET-Anfrage
curl -X GET https://api.example.com/data

# POST-Anfrage mit Payload
curl -X POST -d '{"key":"value"}' https://api.example.com/data
```

### Antwort-Header anzeigen
```Bash
curl -I https://api.example.com/data
```

### HTTP-Statuscode auswerten
```Bash
response=$(curl --write-out %{http_code} --silent --output /dev/null https://api.example.com/data)
echo $response
```
Output:
```
200
```

## Deep Dive
### Historischer Kontext
HTTP-Anfragen existieren seit Anfang der 1990er, als Tim Berners-Lee das HTTP initiierte. Curl kam 1997, ermöglicht einfache Befehlszeilenanfragen.

### Alternativen
- wget: ähnlich wie Curl, aber schlechter für Skripte.
- HTTPie: menschfreundlichere HTTP-Cli, aber weniger verbreitet.
- Powershell (Invoke-RestMethod): auf Windows.

### Implementierungsdetails
- `-X`: definiert die HTTP-Methode.
- `-d`: sendet Daten als POST-Body.
- `-I`: zeigt nur HTTP-Header an.
- `--write-out %{http_code}`: gibt HTTP-Statuscode zurück.
- `--silent --output /dev/null`: unterdrückt normalen Output.

## See Also
- [Curl Dokumentation](https://curl.haxx.se/docs/manual.html)
- [HTTPie GitHub Repository](https://github.com/httpie/httpie)
- [GNU Wget Dokumentation](https://www.gnu.org/software/wget/manual/wget.html)
