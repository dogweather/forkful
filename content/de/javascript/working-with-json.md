---
title:                "Javascript: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## Warum

JSON ist eine gängige Formatierungsmethode für den Austausch von Daten in der Programmierung. Es ist einfach zu lesen und zu schreiben für sowohl Menschen als auch Maschinen, was es zu einem wertvollen Werkzeug macht, um Daten zwischen verschiedenen Systemen zu übertragen. Wenn du als Programmierer:in Daten in deiner Anwendung austauschen musst, ist es wichtig, die Grundlagen von JSON zu kennen.

## Wie man mit JSON arbeitet 

Die Verwendung von JSON in deinem JavaScript-Code ist einfach. Du kannst Daten in JSON-Format online generieren oder mit einem JavaScript-Befehl, wie `JSON.stringify()`, aus JavaScript-Objekten erstellen.

Hier ist ein Beispiel, wie du ein JSON-Objekt in deiner JavaScript-Anwendung erstellst:

```Javascript
// Erstellen eines JavaScript-Objekts
let person = {
    name: "Max",
    alter: 25,
    stadt: "Berlin"
};

// Umwandeln des Objekts in JSON-Format
let jsonPerson = JSON.stringify(person);

// Ausgabe des JSON-Objekts im Terminal
console.log(jsonPerson);
```

Die Ausgabe dieses Codes wird folgendes JSON-Objekt sein:

`{"name":"Max","alter":25,"stadt":"Berlin"}`

Wie du sehen kannst, entspricht das JSON-Format der JavaScript-Objekt-Notation, aber alle Werte werden als Strings gespeichert.

Um ein JSON-Objekt in ein JavaScript-Objekt umzuwandeln, kannst du den Befehl `JSON.parse()` verwenden. Hier ist ein Beispiel:

```Javascript
// Erstellen eines JSON-Objekts
let jsonPerson = `{"name":"Max","alter":25,"stadt":"Berlin"}`;

// Umwandeln des JSON-Objekts in ein JavaScript-Objekt
let person = JSON.parse(jsonPerson);

// Ausgabe des JavaScript-Objekts im Terminal
console.log(person);
```

Die Ausgabe dieses Codes wird folgendes JavaScript-Objekt sein:

`{name: "Max", alter: 25, stadt: "Berlin"}`

Du kannst auch Arrays in JSON-Format erstellen und wieder in JavaScript-Arrays umwandeln. Hier ist ein Beispiel:

```Javascript 
// Erstellen eines JavaScript-Arrays
let hobbies = ["Schwimmen", "Lesen", "Reisen"];

// Umwandeln des Arrays in JSON-Format
let jsonHobbies = JSON.stringify(hobbies);

// Ausgabe des JSON-Arrays im Terminal
console.log(jsonHobbies);
```

Die Ausgabe dieses Codes wird folgendes JSON-Array sein: `["Schwimmen", "Lesen", "Reisen"]`

Und hier ist, wie du ein JSON-Array in ein JavaScript-Array umwandeln kannst:

```Javascript
// Erstellen eines JSON-Arrays
let jsonHobbies = `["Schwimmen", "Lesen", "Reisen"]`;

// Umwandeln des JSON-Arrays in ein JavaScript-Array
let hobbies = JSON.parse(jsonHobbies);

// Ausgabe des JavaScript-Arrays im Terminal
console.log(hobbies);
```

Die Ausgabe dieses Codes wird folgendes JavaScript-Array sein: `["Schwimmen", "Lesen", "Reisen"]`

## Tieferes Eintauchen

Es gibt noch viel mehr, was man über JSON lernen kann, wie zum Beispiel die Verwendung von verschachtelten Objekten oder Arrays, Bedingungen und Schleifen in JSON, und vieles mehr. Es gibt eine Vielzahl von Online-Ressourcen und Tutorials, die dir dabei helfen können, tiefer in die Materie einzutauchen. Eine gute Möglichkeit, das Erlernte zu üben, ist das Schreiben von JSON-Objekten und Arrays in einer Datei und dann ihre Umwandlung in JavaScript-Objekte und Arrays in deiner Anwendung zu verwenden.

## Siehe auch

- [Einführung in JSON von MDN](https://developer.mozilla.org/de/docs/Learn/JavaScript/Objects/JSON)
- [Praktische Anwendung von JSON in JavaScript](https://www.digitalocean.com/community/tutorials/how-to-work-with-json-in-javascript) 
- [Eine interaktive JSON-Lernumgebung](https://jsonmate.com/learn.html)