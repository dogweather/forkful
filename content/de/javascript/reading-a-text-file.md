---
title:                "Einen Text-Datei lesen"
html_title:           "Javascript: Einen Text-Datei lesen"
simple_title:         "Einen Text-Datei lesen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Wenn Du Dich für das Programmieren mit Javascript interessierst, wirst Du wahrscheinlich irgendwann auf Textdateien stoßen. Diese können eine nützliche und wichtige Ressource für Dein Projekt sein. In diesem Artikel werde ich Dir zeigen, wie Du eine Textdatei in Javascript einlesen kannst und was Du damit machen kannst.

##Wie funktioniert es?

Es gibt verschiedene Möglichkeiten, eine Textdatei in Javascript einzulesen. Eine der einfachsten ist die Verwendung der XMLHttpRequest-Methode. Hier siehst Du ein Beispiel:

```Javascript
const xhttp = new XMLHttpRequest();
xhttp.onreadystatechange = function() {
  if (this.readyState == 4 && this.status == 200) {
    console.log(this.responseText);
  }
};
xhttp.open("GET", "beispiel.txt", true);
xhttp.send();
```

In diesem Beispiel wird eine XMLHTTPRequest erstellt, die eine GET-Anfrage an die Datei "beispiel.txt" sendet. Sobald die Anfrage abgeschlossen ist, wird die Funktion onreadystatechange aufgerufen. Wenn der Status der Anfrage erfolgreich ist, wird der Inhalt der Datei mittels console.log ausgegeben.

Es gibt auch andere Methoden, eine Textdatei einzulesen, wie z.B. die Verwendung von fetch oder das Parsen mit dem Node.js FileSystem-Modul. Du kannst die Methode wählen, die am besten zu Deinem Projekt passt.

## Deep Dive

Wenn Du Dich tiefer mit dem Einlesen von Textdateien in Javascript befassen möchtest, gibt es einige wichtige Dinge zu beachten. Zum Beispiel musst Du möglicherweise die Größe der Datei überprüfen oder mit Fehlermeldungen umgehen, falls die Anfrage fehlschlägt.

Außerdem kannst Du mit verschiedenen Methoden die Daten aus der Textdatei extrahieren, z.B. durch Aufteilen in einzelne Zeilen oder Suchen nach bestimmten Mustern.

Es ist auch wichtig zu beachten, dass Javascript standardmäßig keine Zugriffsberechtigung auf Dateien hat, die sich auf dem lokalen System des Nutzers befinden. In diesen Fällen musst Du eventuell eine Backend-Serverumgebung einrichten, um die Datei zugänglich zu machen.

## Siehe Auch

- [XMLHttpRequest](https://developer.mozilla.org/de/docs/Web/API/XMLHttpRequest)
- [fetch](https://developer.mozilla.org/de/docs/Web/API/Fetch_API/Using_Fetch)
- [Node.js FileSystem-Modul](https://nodejs.org/dist/latest-v14.x/docs/api/fs.html)