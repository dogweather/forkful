---
title:                "Eine Webseite herunterladen"
html_title:           "Arduino: Eine Webseite herunterladen"
simple_title:         "Eine Webseite herunterladen"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Herunterladen einer Webseite bedeutet, deren Inhalte auf einen lokalen Gerät abzuspeichern. Programmierer tun dies, um offline arbeiten zu können oder bestimmte Daten zu analysieren und zu verarbeiten.

## So geht's:

Wir verwenden das `net/http` Paket zur Interaktion mit HTTP-Protokoll. Hier ist ein einfaches Beispiel:

```Go
package main

import (
	"fmt"
	"io/ioutil"
	"net/http"
)

func main() {
	resp, err := http.Get("http://example.com/")
	if err != nil {
		fmt.Printf("HTTP request failed: %s\n", err)
		return
	}
	defer resp.Body.Close()
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		fmt.Printf("Read failed: %s\n", err)
		return
	}
	fmt.Printf("%s\n", body) 
}
```

Im obigen Beispiel fordern wir die Beispiel-URL an und lesen den Body der Antwort.

Der Output könnte ungefähr so aussehen:

```Go
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...</body>
</html>
```

## Tiefere Einblicke:

Das Herunterladen von Webseiten hat eine lange Geschichte, die bis zu den frühen Tagen des Internets zurückreicht. Es gibt viele Methoden, dies zu erreichen, was auf die Vielfalt der verfügbaren Technologien zurückzuführen ist.

Alternativen zum `net/http` Paket in Go könnten Drittprogramme wie `curl` oder `wget` sein. Sie führen ähnliche Aufgaben, aber außerhalb des Go-Ökosystems, aus.

In Bezug auf die Implementierungsdetails hängt die genaue Methode des Webseitendownloads stark von der spezifischen Webseite und der verwendeten Technologie ab. Es ist wichtig, die Dokumentation und eventuell vorhandenen Code zu überprüfen, um zu verstehen, wie die Seite funktioniert.

## Siehe auch:

- Go-Dokumentation zum `net/http` Paket: [https://golang.org/pkg/net/http/](https://golang.org/pkg/net/http/)
- Tutorial zum Download von Dateien in Go: [https://golangcode.com/download-a-file-from-a-url/](https://golangcode.com/download-a-file-from-a-url/)
- Ausführlichere Diskussion über HTTP-Clients in Go: [https://medium.com/@nate510/don-t-use-go-s-default-http-client-4804cb19f779](https://medium.com/@nate510/don-t-use-go-s-default-http-client-4804cb19f779)