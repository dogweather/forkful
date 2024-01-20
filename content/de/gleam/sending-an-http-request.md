---
title:                "Eine HTTP-Anforderung senden"
html_title:           "Bash: Eine HTTP-Anforderung senden"
simple_title:         "Eine HTTP-Anforderung senden"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

---

## Was & Warum?

Das Senden einer HTTP-Anfrage ist eine Methode, bei der ein Server aufgefordert wird, Daten bereitzustellen oder zu empfangen. Programmierer verwenden es, um Daten von APIs abzurufen oder komplexe Webanwendungen zu erstellen.

---

## So geht’s:

Erstellen wir zunächst einen einfachen HTTP-Get-Request mit Gleam:

```Gleam
import gleam/http.{HttpClient}

fn main(_args: List(String)) {
let client = HttpClient.start_link()
let _response = client.get("http://example.com")
response.body
}
```

In diesem Block startet Ihr HTTP-Client, sendet eine GET-Anfrage an "http://example.com" und gibt den Körper der Antwort aus.

---

## Vertiefung

HTTP (Hypertext Transfer Protocol) wurde in den 90er Jahren eingeführt, um die Webkommunikation zu erleichtern. Alternative Methoden zur Datenabfrage können Sockets oder direkte Datenbankzugriffe sein. 

Der `HttpClient.start_link()` in Gleam erzeugt einen neuen, asynchronen HTTP-Client. Dies bedeutet, dass Sie gleichzeitig mehrere Anfragen senden und verarbeiten können, ohne auf die Antwort der vorherigen Anfrage warten zu müssen.

---

## Siehe auch

- Gleam-HTTP-Anforderungen dokumentieren: [Link]https://gleam.run/book/tour/http-requests.html)
- Gleam HTTP-Anforderungen GitHub: [Link] (https://github.com/gleam-lang/gleam_http)

Für weitere Informationen besuchen Sie bitte die oben genannten Links.

---