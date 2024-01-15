---
title:                "Das Herunterladen einer Webseite"
html_title:           "Go: Das Herunterladen einer Webseite"
simple_title:         "Das Herunterladen einer Webseite"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Warum
Warum sollte man sich die Mühe machen, eine Webseite herunterzuladen? Nun, es gibt mehrere Gründe. Vielleicht möchtest du eine Kopie einer Webseite als Referenz haben, besonders wenn du daran arbeitest, sie zu verbessern. Oder vielleicht möchtest du einfach offline auf die Inhalte zugreifen können, ohne auf eine Internetverbindung angewiesen zu sein.

## Wie geht's
Lass uns jetzt schauen, wie wir eine Webseite mit Go herunterladen können. Zuerst müssen wir das "net/http" Paket importieren, welches uns Zugriff auf die HTTP-Client-Funktionen von Go gibt. Dann können wir die "Get" Funktion nutzen, die eine HTTP-Anfrage an die angegebene URL sendet und eine Antwort zurückgibt. In unserem Beispiel wollen wir die Webseite von Google herunterladen, also geben wir einfach die URL "http://www.google.com" als Parameter ein.

```
package main 

import (
    "fmt"
    "net/http"
)

func main() {

    // Get request to Google's homepage
    resp, err := http.Get("http://www.google.com")
    if err != nil {
        fmt.Println("Error:", err)
    }
    defer resp.Body.Close()

    // Store response body in a byte slice
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        fmt.Println("Error:", err)
    }

    // Convert byte slice to string and print it
    fmt.Println(string(body))
}
```

Die Ausgabe wird wahrscheinlich sehr umfangreich sein, da es sich um den gesamten HTML-Code von Googles Homepage handelt. Aber keine Sorge, du hast erfolgreich eine Webseite mit Go heruntergeladen!

## Tiefentauchen
Nun, das Herunterladen einer einfachen Webseite mag nicht allzu beeindruckend sein, aber es ist ein wichtiger Schritt, um weiter fortgeschrittene Projekte zu entwickeln. Mit dem "net/http" Paket von Go kannst du nicht nur Daten von einer Webseite herunterladen, sondern auch verschiedene HTTP-Anfragemethoden nutzen, HTTPS-Verbindungen aufbauen und Cookies verwalten. Es ist ein mächtiges Werkzeug, das es dir ermöglicht, komplexe Aufgaben im Zusammenhang mit HTTP-Anfragen zu bewältigen.

## Siehe Auch
- [Offizielle Dokumentation von Go zum "net/http" Paket](https://golang.org/pkg/net/http/)
- [Tutorial: Eine funktionsfähige RESTful API mit Go bauen](https://www.thepolyglotdeveloper.com/2016/07/create-a-simple-restful-api-with-golang/)
- [Open Source Projekt: Go-HTML-Parser](https://github.com/google/go-html-transform)