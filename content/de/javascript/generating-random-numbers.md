---
title:    "Javascript: Zufallszahlen generieren"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum
Javascript ist eine gängige Programmiersprache, die in vielen Bereichen eingesetzt wird. Eine wichtige Funktion dieser Sprache ist die Möglichkeit, Zufallszahlen zu generieren. Dies kann nützlich sein, um Spiele zu erstellen, Tests zu simulieren oder einfach nur zum Spaß. In diesem Blog-Beitrag werde ich erklären, warum das Generieren von Zufallszahlen in Javascript wichtig ist und wie es funktioniert.

## Wie man Zufallszahlen in Javascript generiert
Um Zufallszahlen in Javascript zu generieren, gibt es zwei Hauptmethoden: `Math.random()` und `crypto.getRandomValues()`. Die `Math.random()` Methode gibt eine Zufallszahl zwischen 0 (inklusive) und 1 (exklusive) zurück. Hier ist ein Beispiel:

```Javascript
var randomNum = Math.random();
console.log(randomNum); // Output: 0.456892765
```

Um jedoch eine Zufallszahl in einem bestimmten Bereich zu bekommen, können wir diese Methode mit `Math.floor()` und `Math.ceil()` kombinieren, um die Zahl auf eine ganze Zahl zu runden. Das folgende Beispiel generiert eine zufällige Ganzzahl zwischen 1 und 10:

```Javascript
var randomNum = Math.floor(Math.random() * 10) + 1;
console.log(randomNum); // Output: 7
```

Eine weitere Methode ist die Verwendung von `crypto.getRandomValues()`, die aus der Web Crypto API stammt. Diese Methode gibt kryptografisch sichere Zufallszahlen zurück und ist daher nützlich, wenn es um Sicherheit und Verschlüsselung geht. Hier ist ein Beispiel:

```Javascript
var array = new Uint32Array(1);
crypto.getRandomValues(array);
console.log(array[0]); // Output: 3764842624
```

Es ist wichtig zu beachten, dass beide Methoden nicht wirklich "zufällige" Zahlen generieren, sondern Pseudozufallszahlen basierend auf einem sogenannten "Seed" oder Startwert. Wenn Sie also dieselbe Methode mehrmals aufrufen, erhalten Sie dieselbe Zufallszahlenserie. Um dies zu vermeiden, können Sie den Seed manuell ändern oder die Zeit als Seed nutzen.

## Tieferer Einblick
Es ist wichtig zu verstehen, dass die Zufallszahlen, die in Javascript generiert werden, nicht wirklich zufällig sind. Sie basieren auf einem Algorithmus und sind daher vorhersehbar. Dies ist in Ordnung für viele Anwendungen, aber wenn es um Sicherheit geht, ist es wichtig, eine andere Methode zu verwenden, wie zum Beispiel `crypto.getRandomValues()`.

Es gibt auch Bibliotheken und Frameworks, die erweiterte Funktionen für die Generierung von Zufallszahlen in Javascript bieten. Eine beliebte Bibliothek ist beispielsweise "random-js".

Insgesamt ist das Generieren von Zufallszahlen in Javascript ein nützliches Werkzeug, das in vielfältigen Anwendungen verwendet wird. Es ist wichtig, die verschiedenen Methoden und deren Unterschiede zu verstehen, um die richtige Methode für jede Anwendung auszuwählen.

## Siehe auch
- [MDN Web Docs - Math.random()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [MDN Web Docs - Web Crypto API](https://developer.mozilla.org/en-US/docs/Web/API/Web_Crypto_API)
- [random-js](https://github.com/joelalejandro/random-js)