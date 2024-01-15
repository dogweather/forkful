---
title:                "Eine http Anfrage senden"
html_title:           "Go: Eine http Anfrage senden"
simple_title:         "Eine http Anfrage senden"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Warum

HTTP-Anfragen (oder auch Hypertext Transfer Protocol-Anfragen) sind unerlässlich, um mit verschiedenen Webdiensten zu kommunizieren und Daten auszutauschen. Indem du lernst, wie man eine HTTP-Anfrage in Go sendet, kannst du effektiver und effizienter mit APIs und anderen Webdiensten interagieren.

## Wie Geht's

Um eine HTTP-Anfrage in Go zu senden, brauchen wir das `net/http` Paket. Wir werden auch `fmt` für die Ausgabe verwenden. Lass uns loslegen!

```Go
package main

import (
	"fmt"
	"net/http"
)

func main() {

	// Erstelle ein neues HTTP-Request mit der URL des Ziels
	req, err := http.NewRequest("GET", "https://www.example.com", nil)
	if err != nil {
		fmt.Println("Fehler beim Erstellen der Anfrage:", err)
		return
	}

	// Sende die Anfrage an das Ziels
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		fmt.Println("Fehler beim Senden der Anfrage:", err)
		return
	}
	defer resp.Body.Close()

	// Lese die Antwort des Ziels und gebe sie aus
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		fmt.Println("Fehler beim Lesen der Antwort:", err)
		return
	}
	fmt.Println(string(body))

}
```

Die Ausgabe sollte die HTML-Inhalte der angegebenen URL anzeigen. Beachte, dass wir auch eine Fehlerbehandlung machen, um mögliche Fehler bei der Anfrage und der Verbindung zu erkennen.

## Tiefer Tauchen

Jetzt, da wir wissen, wie man eine grundlegende HTTP-Anfrage sendet, können wir tiefer tauchen und die verschiedenen Arten von Anfragen und ihre Funktionen untersuchen.

Die `http.NewRequest()` Methode, die wir in unserem Code verwendet haben, nimmt drei Argumente an: `method`, `url` und `body`. Mit `method` können wir die Art der Anfrage wie `GET`, `POST`, `PUT`, etc. angeben. Der `url` Parameter sollte die URL des Ziels enthalten und `body` kann verwendet werden, um Daten mitzusenden, wie z.B. beim Senden von Formulardaten.

Wir können auch zusätzliche Header hinzufügen, um unsere Anfrage zu personalisieren, indem wir die `req.Header.Set()` Methode verwenden.

>Weitere Informationen über das `net/http` Paket und die verschiedenen Möglichkeiten, HTTP-Anfragen zu senden, findest du in der offiziellen Dokumentation.

## Siehe Auch

- Offizielle Dokumentation: https://golang.org/pkg/net/http/
- YouTube Tutorial: https://www.youtube.com/watch?v=ccANcNk8Dac
- Medium Artikel: https://medium.com/@masnun/making-http-requests-in-golang-dd123379efe7