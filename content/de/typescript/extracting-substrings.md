---
title:                "TypeScript: Substrings extrahieren"
simple_title:         "Substrings extrahieren"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

#Warum substrings in TypeScript extrahieren?

Das Extrahieren von Substrings ist eine häufige Aufgabe in der Programmierung, besonders in TypeScript. Ob es darum geht, eine bestimmte Zeichenkette aus einem Text herauszufiltern oder die Länge eines Substrings zu berechnen, die Möglichkeiten sind vielfältig. In diesem Blog-Beitrag werden wir uns ansehen, warum und wie man Substrings in TypeScript extrahieren kann.

##Wie man Substrings in TypeScript extrahiert
Um Substrings in TypeScript zu extrahieren, gibt es verschiedene Methoden. Eine Möglichkeit ist die Verwendung der ```substring()``` Methode, die eine Teilzeichenkette aus einem vorhandenen String zurückgibt. Sie akzeptiert zwei Parameter: den Startindex und den Endindex. Der Startindex gibt an, wo der Substring beginnen soll, während der Endindex die Position des letzten Zeichens im Substring angibt.

```TypeScript
let text: string = "Dies ist ein Beispieltext.";

//Extrahieren eines Substrings mit der substring() Methode
let subtext: string = text.substring(5, 17); //Ergebnis: "ist ein Bes" 
```

Eine andere Möglichkeit ist die Verwendung der ```slice()``` Methode. Sie funktioniert ähnlich wie ```substring()```, jedoch akzeptiert sie auch negative Werte für den Startindex und den Endindex. Ein negativer Startindex gibt an, dass der Substring von hinten beginnt, während ein negativer Endindex die Anzahl der Zeichen vom Ende des Strings angibt.

```TypeScript
let text: string = "Dies ist ein Beispieltext.";

//Extrahieren eines Substrings mit der slice() Methode
let subtext: string = text.slice(-5, -1); //Ergebnis: "text" 
```

Es ist auch möglich, Substrings mithilfe von regulären Ausdrücken zu extrahieren. Die ```match()``` Methode nimmt einen regulären Ausdruck als Parameter und gibt ein Array aller Übereinstimmungen zurück. Mit Hilfe von Gruppierung in regulären Ausdrücken kann man auch bestimmte Teile des Strings extrahieren.

```TypeScript
let text: string = "Dies ist ein Beispieltext.";

//Extrahieren eines Substrings mit einem regulären Ausdruck
let subtext: RegExpMatchArray = text.match(/(\w+)text/); //Ergebnis: ["Beispieltext","Beispiel"]
```

##Tiefergehende Informationen über Substrings in TypeScript
Bei der Extrahierung von Substrings in TypeScript gibt es einige Dinge, auf die man achten sollte. Zum Beispiel ist der Endindex in der ```substring()``` Methode exklusiv, was bedeutet, dass das letzte Zeichen, das ausgewählt wird, tatsächlich der Index davor ist. Im Gegensatz dazu ist der Endindex in der ```slice()``` Methode inklusiv, d.h. das letzte angegebene Zeichen ist Teil des Substrings.

Außerdem sollte man bei Verwendung von negativen Werten für den Endindex bedenken, dass ein negativer Wert, der größer ist als der Startindex, zu einem leeren String als Ergebnis führt.

##Siehe auch
- [String Methoden in TypeScript](https://www.typescriptlang.org/docs/handbook/basic-types.html#string-methods)
- [Reguläre Ausdrücke in TypeScript](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)