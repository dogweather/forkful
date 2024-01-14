---
title:    "Javascript: Ein Datum in einen String umwandeln"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Warum

Das Konvertieren von Datumsangaben in Zeichenfolgen ist ein häufiger Schritt beim Programmieren von Webanwendungen. Es ist wichtig zu verstehen, wie man dies richtig macht, um effizient und fehlerfrei zu arbeiten.

# Wie man es macht

Die Javascript-Methode `toString()` ermöglicht es uns, ein Datum in einen lesbareren String umzuwandeln. In der folgenden Code-Beispiellinie sehen wir, wie dies gemacht wird:

```Javascript
const date = new Date(2020, 11, 25);
const dateString = date.toString();
console.log(dateString); // Output: Fri Dec 25 2020 00:00:00 GMT+0100 (Mitteleuropäische Normalzeit)
```

Wie Sie sehen können, gibt `toString()` das Datum in einem vordefinierten Format aus. Allerdings ist dies eventuell nicht immer die gewünschte Darstellung der Datumsangabe. Glücklicherweise bietet Javascript auch die Möglichkeit, das gewünschte Format anzupassen. Hier ist ein Beispiel, das das Datum in dem sehr gebräuchlichen Format `TT.MM.JJJJ` ausgibt:

```Javascript
const date = new Date(2020, 11, 25);
const options = {day: "numeric", month: "2-digit", year: "numeric"};
const dateString = date.toLocaleDateString("de-DE", options);
console.log(dateString); // Output: 25.12.2020
```

Hier verwenden wir die Methode `toLocaleDateString()` mit dem Parameter `"de-DE"`, um sicherzustellen, dass das Datum im deutschen Format ausgegeben wird. Die Optionen ermöglichen uns, das Format des Datums festzulegen, indem wir angeben, welche Teile des Datums (`day`, `month` und `year`) und in welchem Format (hier `numeric` für Zahlen und `2-digit` für zweistellige Zahlen) angezeigt werden sollen.

# Tiefere Einblicke

Javascript bietet uns auch die Möglichkeit, benutzerdefinierte Datumsformate zu erstellen. Dazu verwenden wir die `strftime()`-Methode des `strftime`-Moduls. Hier ist ein Beispiel, das das Datum als deutschen Text mit dem Monat als Abkürzung ausgibt:

```Javascript
const date = new Date(2020, 11, 25);
const dateFormat = '%d. %b %Y';
const strftime = require('strftime');
const dateString = strftime(dateFormat, date);
console.log(dateString); // Output: 25. Dez 2020
```

Hier definieren wir zuerst unser gewünschtes Format `dateFormat` und importieren dann das `strftime`-Modul, das die Methode `strftime()` enthält. Wir geben diese Methode dann mit dem gewünschten Format und dem Datum als Parameter aus. Es gibt viele verschiedene Platzhalter, die verwendet werden können, um benutzerdefinierte Formate zu erstellen, und sie können in offiziellen Dokumentationen nachgelesen werden.

# Siehe auch

- [Javascript Date Referenz](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [strftime Modul Dokumentation](https://github.com/samsonjs/strftime)