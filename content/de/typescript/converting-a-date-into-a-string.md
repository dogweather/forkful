---
title:    "TypeScript: Umwandlung eines Datums in einen String"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren eines Datums in einen String ist eine häufige Aufgabe beim Programmieren. Es ermöglicht es uns, Datumsangaben lesbarer und formatiert auszugeben und sie in verschiedenen Teilen unserer Anwendung zu verwenden. In diesem Blog-Beitrag werden wir uns ansehen, wie man dies in TypeScript tun kann.

## Wie geht das?

Um ein Datum in einen String umzuwandeln, können wir in TypeScript die `toString()`-Methode verwenden. Diese Methode gibt uns eine Zeichenfolge zurück, die das Datum im ISO-Format darstellt. Hier ist ein Beispiel:

```TypeScript
const today = new Date();
const dateString = today.toString();

console.log(dateString);
// Ausgabe: Fri Jul 30 2021 13:41:22 GMT+0300 (Eastern European Summer Time)
```

Wir können die `toString()`-Methode auch mit anderen Date-Objekten konvertieren, indem wir sie als Argument übergeben, zum Beispiel:

```TypeScript
const christmas = new Date(2021, 11, 25);
const christmasString = christmas.toString();

console.log(christmasString);
// Ausgabe: Sat Dec 25 2021 00:00:00 GMT+0200 (Eastern European Standard Time)
```

Es gibt auch andere Methoden, um ein Datum in einen String umzuwandeln, je nachdem, welche Formatierung wir benötigen. Zum Beispiel können wir die `toLocaleDateString()`-Methode verwenden, um ein Datum im lokalen Format zu erhalten:

```TypeScript
const birthday = new Date(1990, 4, 15);
const birthdayString = birthday.toLocaleDateString();

console.log(birthdayString);
// Ausgabe: 15.05.1990
```

## Tiefer Einblick

In TypeScript können wir auch die `format()`-Methode aus der `date-fns`-Bibliothek verwenden, um ein Datum in einem spezifischen Format zu erhalten. Diese Methode nimmt zwei Argumente - das Datum selbst und das gewünschte Format - und gibt einen formatierten String zurück. Zum Beispiel, wenn wir das Datum im Format "TT.MM.JJJJ" haben wollen, können wir folgenden Code verwenden:

```TypeScript
import { format } from 'date-fns'

const date = new Date(2021, 6, 30);
const formattedDate = format(date, 'dd.MM.yyyy');

console.log(formattedDate);
// Ausgabe: 30.07.2021
```

Es gibt noch viele weitere Methoden und Bibliotheken, mit denen man ein Datum in einen String umwandeln kann, je nach individuellen Anforderungen und Vorlieben.

## Siehe auch

- [MDN Web Docs - Date.prototype.toString()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toString)
- [Date-fns Library](https://date-fns.org/)
- [Format.js Library](https://date-fns.org/)