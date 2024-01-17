---
title:                "Herunterladen einer Webseite"
html_title:           "Javascript: Herunterladen einer Webseite"
simple_title:         "Herunterladen einer Webseite"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Herunterladen einer Webseite bedeutet, dass man den Inhalt einer Webseite auf seinen Computer oder ein anderes Gerät herunterlädt. Programmierer tun dies, um den Inhalt einer Webseite für ihre Programme zugänglich zu machen oder um Informationen von der Webseite zu extrahieren.

## Wie geht's?

Das Herunterladen einer Webseite in JavaScript ist relativ einfach. Man kann die `fetch()` Funktion verwenden, um eine Anfrage an die Webseite zu senden und den Inhalt herunterzuladen. Hier ist ein Beispiel:

```Javascript
fetch("https://www.example.com")     // Anfrage an die Webseite senden
  .then(response => response.text())  // Die Antwort in Text umwandeln
  .then(data => console.log(data))    // Ausgabe des heruntergeladenen Inhalts
  .catch(error => console.log(error)) // Fehlerbehandlung
```

Dieses Code-Beispiel verwendet die `.then()` Methode, um die heruntergeladenen Daten in Text umzuwandeln und dann in der Konsole auszugeben. Es gibt auch die Möglichkeit, die Daten in einem JSON-Format herunterzuladen, indem man die `.json()` Methode verwendet.

```Javascript
fetch("https://www.example.com/api")  // Anfrage an die JSON-API senden
  .then(response => response.json())  // Die Antwort in JSON umwandeln
  .then(data => console.log(data))    // Ausgabe der heruntergeladenen Daten
  .catch(error => console.log(error)) // Fehlerbehandlung
```

Es ist auch wichtig zu beachten, dass der Code innerhalb der `.then()` Methode nur ausgeführt wird, wenn die Anfrage erfolgreich war. Bei einem Fehler wird der Code innerhalb der `.catch()` Methode ausgeführt.

## Tiefer tauchen

Das Herunterladen von Webseiten war in der Vergangenheit komplizierter und erforderte die Verwendung von XMLHTTPRequest oder Ajax. Mit der Einführung von `fetch()` in ES6 wurde dies jedoch viel einfacher. Es gibt auch Alternativen wie z.B. die Verwendung von Frameworks wie jQuery, um Anfragen an Webseiten zu senden.

Es ist auch wichtig zu beachten, dass beim Herunterladen einer Webseite viele Aspekte wie Sicherheit, Cookies und Redirects berücksichtigt werden müssen. Die `fetch()` Funktion kümmert sich jedoch standardmäßig um diese Dinge und erleichtert so das Herunterladen von Webseiten.

## Siehe auch

- [MDN Web Docs über die fetch() Funktion](https://developer.mozilla.org/de/docs/Web/API/Fetch_API/Using_Fetch)
- [JQuery Dokumentation](https://jquery.com/)