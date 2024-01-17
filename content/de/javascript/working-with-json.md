---
title:                "Arbeiten mit JSON"
html_title:           "Javascript: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/working-with-json.md"
---

{{< edit_this_page >}}

# Was & Warum?
JSON (Javascript Object Notation) ist ein leichtgewichtiges Datenformat, das von Programmierern verwendet wird, um Daten zu speichern und auszutauschen. Es ist besonders nützlich, da es einfach zu lesen und zu schreiben ist und von vielen Programmiersprachen unterstützt wird.

# Wie geht das?
```Javascript
// Beispiel JSON-Objekt
let person = {
    name: 'Max Mustermann',
    alter: 30,
    hobbies: ['Lesen', 'Reisen', 'Programmieren']
};

// JSON-Objekt in String umwandeln
let json = JSON.stringify(person);
console.log(json);
// Ausgabe: {"name":"Max Mustermann","alter":30,"hobbies":["Lesen","Reisen","Programmieren"]}

// String in JSON-Objekt zurückwandeln
let person2 = JSON.parse(json);
console.log(person2);
// Ausgabe: { name: 'Max Mustermann', alter: 30, hobbies: [ 'Lesen', 'Reisen', 'Programmieren' ] }
```

# Tiefere Einblicke
JSON wurde von Douglas Crockford im Jahr 2001 entwickelt und basiert auf der Javascript-Syntax. Es ist eine alternative Methode zur Speicherung von Daten im Vergleich zu XML oder CSV. JSON wird häufig im Web verwendet, um Daten zwischen Client und Server auszutauschen.

# Weitere Quellen
- [JSON auf Wikipedia](https://de.wikipedia.org/wiki/JavaScript_Object_Notation)
- [offizielle JSON-Website](https://www.json.org/json-de.html)
- [Einführung in JSON von MDN](https://developer.mozilla.org/de/docs/Learn/JavaScript/Objects/JSON)