---
title:                "TypeScript: Ein Datum in einen String umwandeln"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Die Umwandlung von einem Datum in einen String ist ein wichtiges Konzept in der Programmierung, da es uns ermöglicht, das Datum lesbar und verständlich für den Benutzer anzuzeigen. Dies ist besonders nützlich beim Erstellen von Datumsauswahlfeldern oder bei der Ausgabe von Daten in bestimmten Formaten.

## Wie geht man vor

Die Umwandlung eines Datums in einen String kann in TypeScript mithilfe der "toString" Funktion erfolgen. Diese Methode nimmt ein Datum als Parameter und gibt einen String zurück, der das Datum in einem bestimmten Format darstellt. Hier ist ein Beispiel, wie man dies in TypeScript machen kann:

```TypeScript
let today = new Date();
let dateString = today.toString();
console.log(dateString);
```

Das obige Beispiel erstellt ein neues Date-Objekt, welches das aktuelle Datum und Uhrzeit enthält. Dann wird die "toString" Funktion aufgerufen und das Ergebnis in der Variablen "dateString" gespeichert. Schließlich wird der Inhalt der Variablen in der Konsole ausgegeben. Der Code würde folgende Ausgabe erzeugen:

`Tue Apr 06 2021 20:14:57 GMT+0200 (Central European Summer Time)`

Dies ist jedoch nur ein Beispiel für die Standardausgabe des Datums. Sie können auch das Format des Strings anpassen, indem Sie der "toString" Methode verschiedene Parameter übergeben. Zum Beispiel könnten Sie das Datum im ISO-Format ausgeben, indem Sie die folgende Zeile verwenden:

`today.toString("yyyy-MM-dd")`

Dies würde die Ausgabe als "2021-04-06" erzeugen.

## Tiefergehende Informationen

Es gibt verschiedene Möglichkeiten, ein Datum in einen String zu konvertieren. Eine Alternative zur "toString" Methode ist das Verwenden von Formatierungsmethoden wie "toLocaleDateString" oder "toLocaleTimeString". Diese bieten mehr Kontrolle über das genaue Format des Strings basierend auf der Region und den Einstellungen des Benutzers.

Darüber hinaus gibt es auch Bibliotheken wie Moment.js, die es einfacher machen, komplexe Datumsformatierungen durchzuführen.

Es ist auch wichtig zu beachten, dass bei der Umwandlung von einem Datum in einen String auch die Zeitzone berücksichtigt werden muss. Dies kann zu unerwarteten Ergebnissen führen, wenn man nicht darauf achten.

## Siehe auch

- [MDN Web Docs - Date.prototype.toString()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toString)
- [Moment.js Dokumentation](https://momentjs.com/docs/#/displaying/)
- [W3Schools - Date Methods](https://www.w3schools.com/js/js_date_methods.asp)