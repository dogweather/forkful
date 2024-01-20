---
title:                "Eine HTTP-Anforderung senden"
html_title:           "Bash: Eine HTTP-Anforderung senden"
simple_title:         "Eine HTTP-Anforderung senden"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?

Beim Senden einer HTTP-Anforderung kommuniziert man mit Web-Servern. Dies ist nützlich, um Daten zu übertragen, APIs anzusprechen oder Webseiten-Inhalte zu laden.

## So geht's:

Wirsenden ein HTTP GET Request mit `curl`. Das ist der Code:
```Bash
#!/bin/bash
url='http://example.com'
curl $url
```

Der Output kann so aussehen:
```Bash
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

Ein POST Request mit Daten könnte so ausssehen:
```Bash
#!/bin/bash
url='http://example.com'
data='{"name":"John", "age":30}'

curl -X POST -d "$data" $url -H "Content-Type: application/json"
```

## Vertiefung

Erste HTTP Anfragen waren in den 90er Jahren möglich. `curl` und `wget` sind dafür beliebte, weil einfache Tools. Ein alternativer Weg ist die Verwendung von Bibliotheken in Programmiersprachen wie Python's `requests` oder Node.js `http`.

Der `curl` Befehl sendet standardmäßig GET Requests. Mit `-X POST` ändern wir den HTTP Verbs zu POST. Der `-d` Parameter gibt die zu sendenden Daten an. `-H` setzt den Content-Type Header auf `application/json`.

## Siehe Auch

Weiterführende Infos:
- `curl` Manpage: https://curl.se/docs/manpage.html
- HTTP Spezifikation: https://tools.ietf.org/html/rfc2616