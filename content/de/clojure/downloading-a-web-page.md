---
title:                "Eine Webseite herunterladen"
html_title:           "Clojure: Eine Webseite herunterladen"
simple_title:         "Eine Webseite herunterladen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte jemand eine Webseite herunterladen? Nun, es gibt viele Gründe! Manchmal möchte man eine Kopie einer Seite für den Offline-Zugriff haben, andere Male möchte man sie untersuchen und möglicherweise Daten extrahieren. Egal aus welchem Grund, das Herunterladen einer Webpage ist eine nützliche Fähigkeit, die jeder Programmierer in seinem Werkzeugkasten haben sollte.

## Wie geht das?

Um eine Webseite herunterzuladen, müssen wir zunächst die URL kennen. Diese ist normalerweise in der Adressleiste des Browsers zu finden. Wir können dann die `clojure.java.io` Bibliothek verwenden, um die Seite herunterzuladen und den Inhalt auf der Konsole auszugeben.

```Clojure
(require '[clojure.java.io :as io])

(def url "https://www.beispielwebseite.com/")

(def page (slurp url))
(io/copy (io/file "heruntergeladene-webpage.html") page)
```

Die `slurp` Funktion lädt den Inhalt der URL als String herunter, und die `copy` Funktion speichert diesen Inhalt in einer Datei namens "heruntergeladene-webpage.html". Nun können wir diese Datei öffnen und den Inhalt anzeigen.

```
<!DOCTYPE html>
<html>
  <head>
    <title>Beispielwebseite</title>
  </head>
  <body>
    <h1>Willkommen auf unserer Webseite!</h1>
    <p>Hier finden Sie alle Informationen, die Sie brauchen.</p>
  </body>
</html>
```

## Tiefere Einsicht

Das Herunterladen einer Webseite mag einfach erscheinen, aber es gibt viele Dinge, die dabei passieren. Beim Herunterladen wird ein HTTP-Request an den Server geschickt, der dann den Inhalt der Seite als HTTP-Response zurücksendet. Die Daten werden dann in einen String konvertiert und an unsere `page` Variable gebunden.

Es gibt auch andere Möglichkeiten, eine Webseite herunterzuladen, wie zum Beispiel mit der Bibliothek `clj-http` oder dem Leiningen-Plugin `lein-fetch`. Diese Methoden bieten zusätzliche Funktionen wie das Herunterladen von Bildern oder die Angabe von Benutzernamen und Passwörtern für Webseiten, die eine Authentifizierung erfordern.

## Siehe auch

- [clojure.java.io Dokumentation] (https://clojure.github.io/clojure/clojure.java.io-api.html)
- [clj-http Dokumentation] (https://github.com/dakrone/clj-http)
- [Leiningen-Plugin "lein-fetch"] (https://github.com/martinklepsch/lein-fetch)