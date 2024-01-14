---
title:    "Javascript: Umwandlung eines Datums in eine Zeichenfolge"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Warum

Das Konvertieren von einem Datum in einen String ist eine grundlegende Funktion in der Javascript Programmierung, die oft im Alltag vorkommt. Es ermöglicht uns, ein Datum in einem für Menschen lesbaren Format darzustellen und ist daher eine wichtige Fähigkeit für jeden Javascript Entwickler.

## Wie es gemacht wird

Um ein Datum in einen String umzuwandeln, gibt es mehrere Möglichkeiten. Eine einfache Möglichkeit ist die Verwendung der `Date()` Funktion, die ein Datum-Objekt zurückgibt. Dieses Objekt kann dann mithilfe von Methoden wie `getFullYear()` und `getMonth()` in ein lesbares Format umgewandelt werden.

```javascript
let date = new Date();
let year = date.getFullYear();
let month = date.getMonth() + 1; // Hinweis: Monate in Javascript beginnen bei 0, daher müssen wir 1 addieren
let day = date.getDate();

// Ausgabe: 02.11.2021
console.log(`${day < 10 ? "0" + day : day}.${month < 10 ? "0" + month : month}.${year}`);
```

Eine weitere Möglichkeit ist die Verwendung der `toLocaleDateString()` Methode, die es uns ermöglicht, das Datum in einem bestimmten Landesformat darzustellen. Zum Beispiel:

```javascript
let date = new Date();
let options = { year: "numeric", month: "long", day: "numeric" };

// Ausgabe für Deutschland: 2. November 2021
console.log(date.toLocaleDateString("de-DE", options));
```

## Tiefergehende Informationen

Es gibt verschiedene Aspekte, die bei der Konvertierung eines Datums in einen String zu beachten sind. Zum Beispiel die Formatierungsoptionen, die von der `toLocaleDateString()` Methode unterstützt werden, oder die Berücksichtigung von Zeitzone und Sprache bei der `Date()` Funktion. Es ist wichtig, diese Aspekte zu verstehen, um genaue und konsistente Ergebnisse zu erhalten.

Eine weitere wichtige Sache ist, dass Javascript standardmäßig die lokale Zeitzone des Benutzers verwendet. Wenn du ein Datum in einer bestimmten Zeitzone darstellen möchtest, musst du dies explizit angeben. Zum Beispiel:

```javascript
let date = new Date();
let options = { timeZone: "Europe/Berlin" };

// Ausgabe: 02.11.2021, 10:00 Uhr (abhängig von deiner aktuellen Zeit)
console.log(date.toLocaleDateString("de-DE", options));
```

## Siehe auch

- [MDN Dokumentation über das Konvertieren von Datum in String](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString)
- [Die `Date()` Funktion in der offiziellen Javascript Dokumentation](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Leitfaden zur Verwendung von Zeit und Datum in Javascript](https://flaviocopes.com/javascript-dates/)

Vielen Dank fürs Lesen und viel Spaß beim Konvertieren von Datum in String in deinen Javascript Projekten!