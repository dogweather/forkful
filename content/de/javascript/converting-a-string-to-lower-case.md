---
title:                "Einen String in Kleinbuchstaben umwandeln"
html_title:           "Elm: Einen String in Kleinbuchstaben umwandeln"
simple_title:         "Einen String in Kleinbuchstaben umwandeln"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Zeichenketten in Kleinbuchstaben umbenennen: Ein JavaScript-Leitfaden

## Was & Warum?

Das Umwandeln einer Zeichenkette in Kleinbuchstaben bedeutet, jeden Großbuchstaben in der Zeichenkette in seinen entsprechenden Kleinbuchstaben zu ändern. Dies ist nützlich, wenn wir bei Vergleichen und Suchvorgängen Groß- und Kleinschreibung ignorieren wollen.

## Wie geht das?

In JavaScript ist das Umwandeln einer Zeichenkette in Kleinbuchstaben denkbar einfach. Man verwendet dazu einfach die Methode `toLowerCase()`:

```Javascript
let str = "Hallo Welt!";
let lowerCaseStr = str.toLowerCase();
console.log(lowerCaseStr); // "hallo welt!"
```

Die Methode `toLowerCase()` ändert die Ausgangszeichenkette nicht, sondern gibt eine neue Zeichenkette zurück, die aus denselben Buchstaben besteht, aber alle in Kleinbuchstaben.

## Vertiefung 

Historisch gesehen ist die `toLowerCase()` Methode seit der ersten Version von ECMAScript, dem Standard, auf dem JavaScript basiert, vorhanden. Es gibt keine wirklichen Alternativen zur `toLowerCase()` Methode in JavaScript selbst, es sei denn, Sie programmieren eine eigene Funktion, um diese Aufgabe zu erfüllen. 

In Bezug auf die Implementierungsdetails ist zu beachten, dass die `toLowerCase()` Methode nicht die Ausgangszeichenkette ändert. Das bedeutet, dass sie "unveränderlich" ist (in der Fachsprache "immutable"). Dies ist eine wichtige Eigenschaft von Zeichenketten in vielen modernen Programmiersprachen, einschließlich JavaScript.

## Siehe auch

- [JavaScript String toLowerCase() Methode auf MDN Web Docs](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [JavaScript Zeichenketten Referenz auf W3Schools](https://www.w3schools.com/jsref/jsref_obj_string.asp)

Hoffentlich gibt Ihnen dieser kurze Leitfaden einen guten Überblick und praktisches Wissen über das Umwandeln einer Zeichenkette in Kleinbuchstaben in JavaScript. Viel Spaß beim Coden!