---
title:                "Eine Webseite herunterladen"
html_title:           "Arduino: Eine Webseite herunterladen"
simple_title:         "Eine Webseite herunterladen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Herunterladen einer Webseite bedeutet, ihre Inhalte auf unserer lokalen Maschine zu speichern. Programmierer tun dies oft, um Daten zu analysieren, Inhalte zu scrapen oder offline auf die Seite zuzugreifen.

## So geht’s:

Es gibt verschiedene Möglichkeiten, um eine Webseite mithilfe von JavaScript zu laden. Eine einfache Methode ist die Verwendung des Standard Fetch API. Hier ist ein einfaches Beispiel:

```Javascript
fetch("https://example.com")
.then(response => response.text())
.then(data =>
{
    console.log(data);
})
.catch(error => console.log('Fehler beim Laden der Seite: ' + error));
```

Wenn Sie diesen Code ausführen, wird der Inhalt von "https://example.com" in der Konsole gedruckt.

## Tiefere Einsichten

Das Konzept des Herunterladens von Webseiten existiert seit der Gründung des Internets. Im Laufe der Zeit sind jedoch verschiedene Methoden zum Herunterladen von Webseiten entwickelt worden, von denen jede unterschiedliche Verwendungszwecke hat. Das Fetch API, das in unserem Beispiel verwendet wurde, wurde vergleichsweise kürzlich eingeführt und bietet eine praktische Schnittstelle zum Herunterladen von Seiten und Verwalten von HTTP-Anfragen. Es gibt jedoch auch andere Funktionen wie XMLHttpRequest, die noch heute verwendet werden, aber aufgrund ihrer Verbose-Natur an Beliebtheit verloren haben.

Es ist wichtig zu beachten, dass das Herunterladen von Webseiten durch Programmiersprachen wie JavaScript rechtlich und ethisch komplex sein kann. Einige Websites erlauben dies vielleicht nicht, und es ist immer ratsam, vorher die Erlaubnis zu prüfen.

## Weitere Hinweise:

- [Fetch API auf MDN](https://developer.mozilla.org/de/docs/Web/API/Fetch_API)
- [Axios auf GitHub](https://github.com/axios/axios), ein beliebtes HTTP-Client-Paket für JavaScript
- [Was ist Web-Scraping?](https://de.wikipedia.org/wiki/Screen_Scraping)
- [Ethik des Web-Scrapings](https://towardsdatascience.com/ethics-in-web-scraping-b96b18136f01)