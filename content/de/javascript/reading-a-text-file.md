---
title:                "Javascript: Ein Textdokument lesen"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von Textdateien ist ein wichtiger Bestandteil der Programmierung in Javascript. Es ermöglicht uns, Daten aus externen Quellen zu lesen und sie in unseren Code zu integrieren. Das kann hilfreich sein, um zum Beispiel Konfigurationsdateien zu lesen oder Benutzereingaben zu verarbeiten.

## How To

Um eine Textdatei in Javascript zu lesen, gibt es mehrere Schritte, die wir befolgen müssen. Zunächst müssen wir eine Verbindung zu der Textdatei herstellen. Das kann auf verschiedene Arten geschehen, zum Beispiel durch das Erstellen eines Objekts vom Typ `FileReader`. Anschließend müssen wir die Datei öffnen, indem wir die `open()` Methode aufrufen. Innerhalb dieser Methode können wir angeben, wie wir den Inhalt der Datei lesen möchten. Zum Beispiel können wir die Datei zeilenweise lesen oder den gesamten Inhalt auf einmal.

Um den Inhalt der Datei zu lesen, verwenden wir die `read()` Methode. Diese liest die nächsten Zeichen aus der Datei und gibt sie als String zurück. Es ist wichtig, den Rückgabewert dieser Methode sorgfältig zu verarbeiten, da er möglicherweise nicht dem gewünschten Ergebnis entspricht. Wenn wir den gesamten Inhalt der Datei lesen möchten, müssen wir sicherstellen, dass wir die `read()` Methode so oft aufrufen, bis wir das Ende der Datei erreicht haben.

Um ein Beispiel zu sehen, schauen wir uns den folgenden Code an:

```javascript
let reader = new FileReader();

// verbinde mit der Datei
reader.open("data.txt");

// lese die Datei zeilenweise
while(true) {
  let line = reader.read();
  if(line == null) {
    // Dateiende erreicht
    break;
  }
  console.log(line); // gebe Zeile in Konsole aus
}
```

Die `while` Schleife wird solange ausgeführt, bis das Ende der Datei erreicht ist. In jeder Schleifeniteration wird eine Zeile aus der Datei gelesen und in der Konsole ausgegeben. Dies ermöglicht es uns, den Inhalt der Datei weiter zu verarbeiten, zum Beispiel indem wir ihn in einer Variablen speichern oder ihn in ein Objekt parsen.

## Deep Dive

Das Lesen von Textdateien erfordert ein grundlegendes Verständnis von Dateisystemen und Datenverarbeitung. Es ist wichtig zu beachten, dass die meisten Funktionen, die im Zusammenhang mit dem Lesen von Textdateien stehen, asynchron ausgeführt werden. Das bedeutet, dass der Code nicht unbedingt in der Reihenfolge ausgeführt wird, wie er im Skript geschrieben wurde. Stattdessen werden diese Funktionen "hinter den Kulissen" ausgeführt, während der restliche Code weiterläuft.

Es ist daher wichtig, asynchrone Funktionen zu verstehen und richtig zu verwenden, um sicherzustellen, dass der Code wie erwartet funktioniert. Außerdem ist es hilfreich, sich mit den verschiedenen Methoden zum Lesen von Textdateien vertraut zu machen, wie zum Beispiel `readAsText()` oder `readAsArrayBuffer()`. Diese bieten unterschiedliche Möglichkeiten, den Inhalt der Datei zu lesen und weiterzuverarbeiten.

## Siehe auch

Hier sind einige weitere Ressourcen, die Ihnen dabei helfen können, das Lesen von Textdateien in Javascript besser zu verstehen:

- [MDN Web Docs: FileReader](https://developer.mozilla.org/de/docs/Web/API/FileReader)
- [Tutorialspoint: Javascript Textdatei lesen](https://www.tutorialspoint.com/javascript-program-dos-file-reading)
- [Stack Overflow: Asynchrone Funktionen verstehen](https://stackoverflow.com/questions/14220321/how-do-i-return-the-response-from-an-asynchronous-call)