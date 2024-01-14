---
title:                "TypeScript: HTML analysieren."
simple_title:         "HTML analysieren."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## Warum

Warum sollten Sie sich mit dem Parsen von HTML beschäftigen? Ganz einfach: HTML ist die Sprache, in der die meisten Webseiten geschrieben sind. Um diese Seiten effektiv zu analysieren und Daten aus ihnen zu extrahieren, ist das HTML-Parsing ein unverzichtbarer Schritt.

## Wie geht man vor?

Um HTML zu parsen, benötigen Sie eine Programmiersprache, die in der Lage ist, Strings und reguläre Ausdrücke zu verarbeiten. Glücklicherweise bietet TypeScript diese Funktionen und macht es somit zu einer idealen Wahl für das HTML-Parsing.

Um einen kleinen Einblick in das Parsen von HTML mittels TypeScript zu bekommen, betrachten wir folgendes Beispiel:

```TypeScript
const htmlString = "<h1>Hello, World!</h1>";
const regex = /<(.*?)>.*<\/(?:.*?)>/;
const matches = regex.exec(htmlString);

console.log("Inhalt des h1-Tags: " + matches[1]); // Ausgabe: Hello, World!
```

In diesem Beispiel wird zuerst ein String mit dem HTML-Code erstellt. Dann wird mittels eines regulären Ausdrucks der Inhalt eines h1-Tags extrahiert und mithilfe von `console.log()` ausgegeben. Wie Sie sehen, ist das Parsen von HTML mit TypeScript relativ einfach.

## Tiefergehende Informationen

Wenn wir tiefer in das Thema einsteigen, gibt es einige wichtige Dinge zu beachten. Zum Beispiel müssen Sie wissen, dass HTML-Dokumente nicht immer perfekt formatiert sind und somit der reguläre Ausdruck möglicherweise angepasst werden muss, um alle gewünschten Ergebnisse zu erhalten. Auch müssen Sie verschiedene HTML-Tags und deren Eigenschaften kennen, um genau die Daten zu extrahieren, die Sie benötigen.

Ein weiterer wichtiger Aspekt ist die Skalierbarkeit. Wenn Sie HTML von mehreren Webseiten parsen möchten, empfiehlt es sich, eine Funktion zu erstellen, die den regulären Ausdruck anpasst und beim Aufruf bestimmte Tags und Eigenschaften erwartet. Dadurch lässt sich der Code flexibler und wartungsfreundlicher gestalten.

## Siehe auch

- [Reguläre Ausdrücke in TypeScript](https://typescriptlang.org/docs/handbook/regular-expressions.html)
- [HTML-Tags und Attribute](https://www.w3schools.com/tags/)
- [Scraping Websites mit TypeScript](https://blog.bitsrc.io/web-scraping-in-node-js-without-a-framework-ec4e851fff3)