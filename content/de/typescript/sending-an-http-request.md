---
title:                "Eine HTTP-Anforderung senden"
html_title:           "Bash: Eine HTTP-Anforderung senden"
simple_title:         "Eine HTTP-Anforderung senden"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?

HTTP-Anfragen dienen dazu, Daten zwischen Client und Server auszutauschen. Programmierer nutzen sie, um Interaktionen mit Web-APIs zu ermöglichen und Daten von externen Quellen abzurufen.

## So Geht's

In TypeScript können wir die eingebaute `fetch` Funktionalität nutzen, um HTTP-Anfragen zu senden:

```TypeScript
let url = "https://api.example.com/data"
fetch(url)
    .then(response => response.json())
    .then(data => console.log(data))
    .catch((error) => console.error('Error:', error));
```

Die Anfrage ruft die URL auf und gibt die empfangenen Daten in der Konsole aus.

## Vertiefung

Im historischen Kontext ist `XMLHttpRequest` das erste native Browser-Objekt für HTTP-Anfragen. Mit der Einführung von `fetch` ist dieser Vorgang jedoch viel sauberer und einfacher geworden.

Alternativen zu `fetch` umfassen Bibliotheken wie `axios` oder `jquery`, welche ähnliche Funktionalität bereitstellen, aber einige zusätzliche Funktionen und Annehmlichkeiten bieten können.

Think also that the `fetch` function returns a Promise that resolves to the Response of the request, whether it is successful or not. It is also important to handle errors and exceptions.

## Weiterführende Informationen

- [MDN-Dokumentation zu Fetch](https://developer.mozilla.org/de/docs/Web/API/Fetch_API/Using_Fetch)
- [Axios-Dokumentation](https://axios-http.com/docs/intro)
- [jQuery-Dokumentation zu Ajax](https://api.jquery.com/jquery.ajax/)