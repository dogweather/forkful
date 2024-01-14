---
title:                "Go: Herunterladen einer Webseite"
simple_title:         "Herunterladen einer Webseite"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Warum

Das Herunterladen einer Webseite kann aus verschiedenen Gründen nützlich sein, zum Beispiel um Daten zu extrahieren oder um Dateien zu sichern. In diesem Blog-Post werde ich dir zeigen, wie du mit Go eine Webseite herunterladen kannst.

## Wie

Das Herunterladen einer Webseite mit Go ist relativ einfach. Alles was du brauchst, ist die "net/http" Paket und die "io/ioutil" Bibliothek. Hier ist ein Beispiel, um die Webseite von Google herunterzuladen:

```
package main

import (
	"net/http"
	"io/ioutil"
	"fmt"
)

func main() {
	resp, err := http.Get("https://www.google.de")
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()
	
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		panic(err)
	}
	
	fmt.Println(string(body))
}
```

Dieser Code sendet eine GET-Anfrage an die URL und liest anschließend den Body der Antwort aus und gibt ihn als String aus.

## Deep Dive

Das Herunterladen einer Webseite kann etwas komplexer werden, wenn du authentifizierte Anfragen oder Cookies benötigst. In solchen Fällen musst du unter Umständen zusätzliche Header hinzufügen und die Anfrage über ein Cookie-Jar verwalten.

Eine weitere Herausforderung beim Herunterladen von Webseiten sind Redirects. Oftmals führen Links auf einer Webseite zu einer anderen URL oder Seite. In solchen Fällen musst du möglicherweise mehrere HTTP-Anfragen senden, um die vollständige Seite zu erhalten.

## Siehe auch

- [net/http Paket in der Go Dokumentation](https://golang.org/pkg/net/http/)
- [ioutil Bibliothek in der Go Dokumentation](https://golang.org/pkg/io/ioutil/)
- [Cookie-Jar Paket in der Go Dokumentation](https://golang.org/pkg/net/http/cookiejar/)