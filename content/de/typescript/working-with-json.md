---
title:                "Arbeiten mit Json"
html_title:           "TypeScript: Arbeiten mit Json"
simple_title:         "Arbeiten mit Json"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## Warum

Wenn du dich in der Webentwicklung oder in der Anwendungsentwicklung befindest, wirst du sicher schonmal von JSON gehört haben. JSON steht für JavaScript Object Notation und ist ein weit verbreitetes Austauschformat für Daten zwischen Anwendungen. Es kann in vielen Programmiersprachen genutzt werden, einschließlich TypeScript, um Daten in einem einfach zu lesenden und schreibbaren Format zu speichern und zu übertragen.

## Wie geht das

Um JSON in TypeScript zu verwenden, können wir die `JSON`-Klasse verwenden, die bereits in TypeScript enthalten ist. Schauen wir uns einige Beispiele an, wie wir JSON-Objekte erstellen und manipulieren können:

```TypeScript
// Ein Objekt erstellen
let person = {
    name: "Max Mustermann",
    alter: 30,
    beruf: "Entwickler"
};

// Objekt in JSON-String umwandeln
let json = JSON.stringify(person);

// Ausgabe: {"name":"Max Mustermann","alter":30,"beruf":"Entwickler"}
console.log(json);

// JSON-String in ein Objekt umwandeln
let personParsed = JSON.parse(json);

// Ausgabe: Max Mustermann
console.log(personParsed.name);
```

In diesem Beispiel erstellen wir ein einfaches Objekt mit den Eigenschaften Name, Alter und Beruf. Mit Hilfe der `JSON.stringify`-Funktion können wir das Objekt in einen JSON-String umwandeln, der dann in einer Datei oder über das Internet übertragen werden kann. Wenn wir den JSON-String erhalten, können wir ihn mit der `JSON.parse`-Funktion wieder in ein Objekt umwandeln und die Eigenschaften abrufen.

## Tiefergehende Informationen

In TypeScript können wir nicht nur einfache Objekte in JSON-Strings umwandeln, sondern auch komplexere Strukturen wie Arrays und verschachtelte Objekte. Hier ist ein Beispiel:

```TypeScript
let autos = [
    {
        marke: "VW",
        farbe: "blau"
    },
    {
        marke: "Audi",
        farbe: "rot"
    },
    {
        marke: "BMW",
        farbe: "grün"
    }
];

let json = JSON.stringify(autos);

// Ausgabe: [{"marke":"VW","farbe":"blau"},{"marke":"Audi","farbe":"rot"},{"marke":"BMW","farbe":"grün"}]
console.log(json);

// Mit forEach über das Array iterieren und Werte ausgeben
// Ausgabe: VW, rot
for (let auto of autos) {
    console.log(auto.marke, auto.farbe);
}
```

Wenn wir ein Array in einen JSON-String umwandeln, bleibt die Reihenfolge der Elemente erhalten. Wir können auch über das Array iterieren, indem wir die Eigenschaften der JSON-Objekte abrufen und ausgeben.

## Siehe auch

- [JSON in TypeScript](https://www.typescriptlang.org/docs/handbook/basic-types.html#json)
- [JSON-Spezifikation](https://www.json.org/)