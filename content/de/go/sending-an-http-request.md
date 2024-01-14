---
title:                "Go: Das Versenden einer HTTP-Anfrage"
simple_title:         "Das Versenden einer HTTP-Anfrage"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Warum

HTTP-Anfragen sind ein grundlegender Bestandteil moderner Webentwicklung. Sie ermöglichen es uns, Daten von anderen Websites abzurufen und unsere eigenen Daten an andere Websites zu senden. In diesem Blogbeitrag werden wir uns damit beschäftigen, wie wir in Go HTTP-Anfragen senden können.

## Wie geht das?

Die `net/http` Paket in Go stellt uns eine Vielzahl von Funktionen zur Verfügung, um HTTP-Anfragen zu senden. Zuerst müssen wir ein `http.Client` Objekt erstellen, das alle nötigen Einstellungen für unsere Anfrage enthält. Dann können wir die `http.NewRequest()` Funktion nutzen, um eine neue Anfrage mit der gewünschten HTTP-Methode, URL und weiteren Parametern zu erstellen.

```Go
// Erstelle ein HTTP-Client Objekt
client := &http.Client{}

// Erstelle eine neue Anfrage
req, err := http.NewRequest("GET", "https://blog.example.com/post/1", nil)
if err != nil {
    panic(err)
}

// Füge Optionen hinzu, z.B. Header oder Query-Parameter
req.Header.Add("Authorization", "Token abc123")
req.URL.Query().Add("tag", "golang")

// Sende die Anfrage und erhalte eine Antwort
resp, err := client.Do(req)
if err != nil {
    panic(err)
}
defer resp.Body.Close()

// Lies die Antwort und gib die Daten aus
body, err := ioutil.ReadAll(resp.Body)
if err != nil {
    panic(err)
}
fmt.Println(string(body))
```

Die oben gezeigten Codebeispiele sind nur ein kleiner Vorgeschmack auf die möglichen Funktionen und Optionen. Für weitere Details über die Verwendung des `net/http` Pakets können Sie die offizielle Dokumentation lesen.

## Tiefere Einblicke

Es ist auch möglich, benutzerdefinierte HTTP-Anfragen zu erstellen, abhängig von unseren spezifischen Anforderungen. Hierfür können wir die `http.Request` Struktur verwenden, um unsere Anfrage zu konfigurieren und dann die `http.Client` Funktionen nutzen, um die Anfrage zu senden.

Es ist natürlich auch wichtig, mögliche Fehler beim Senden von HTTP-Anfragen zu berücksichtigen und entsprechende Fehlerbehandlungen in unseren Code aufzunehmen. Zudem sollten wir uns mit den verschiedenen HTTP-Statuscodes vertraut machen, um angemessen auf die erhaltenen Antworten zu reagieren.

## Siehe auch

- Offizielle Dokumentation zum `net/http` Paket in Go: https://golang.org/pkg/net/http/
- Eine ausführliche Anleitung zum Senden von HTTP-Anfragen in Go mit verschiedenen Beispielen: https://tutorialedge.net/golang/http-go-tutorial/