---
title:                "Arbeiten mit JSON"
html_title:           "Bash: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/working-with-json.md"
---

{{< edit_this_page >}}

## Was & Warum?

JSON steht für JavaScript Object Notation und ist ein gängiges Format zum Speichern und Austauschen von Daten zwischen verschiedenen Programmiersprachen. Programmierer benutzen JSON, um strukturierte Daten zu speichern und zu übertragen, da es leicht lesbar und kompakt ist.

## Wie geht's?

Um mit JSON in Bash zu arbeiten, können wir das integrierte Tool "jq" verwenden. Hier ein Beispiel, wie wir Daten aus einer JSON-Datei filtern und anzeigen können:

```Bash 
json='{"name": "Max", "age": 25, "hobby": "programming"}'
echo $json | jq '.name'
```
Dies würde uns den Wert des "name" Schlüssels ausgeben: Max

Um eine JSON-Datei zu erstellen, können wir in Bash auch Arrays benutzen, wie in diesem Beispiel:

```Bash
array=($(echo '{"name": "Anna", "age": 30, "hobby": "reading"}' | jq -r '.'))
echo "Name: ${array[1]}"
```
Dies würde uns den Wert des "name" Schlüssels ausgeben: Anna

## Tiefer Einblick

JSON wurde ursprünglich von Douglas Crockford erstellt und ist seit 2001 ein offener Standard. Es ist eine Alternative zu XML und hat sich aufgrund seiner Einfachheit und Lesbarkeit schnell als beliebtes Datenformat etabliert.

Obwohl "jq" das bekannteste Tool zur Bearbeitung von JSON in Bash ist, gibt es auch andere Möglichkeiten, wie z.B. die Verwendung von regulären Ausdrücken oder der "jsawk" Bibliothek.

Bei der Arbeit mit JSON in Bash ist es wichtig, darauf zu achten, dass es sich um eine Textverarbeitung handelt und nicht um eine Datenbank. Es kann hilfreich sein, eine Validierung der Daten durchzuführen, bevor sie verarbeitet werden, um unerwartete Fehler zu vermeiden.

## Siehe auch

Weitere Informationen und Beispiele zur Verwendung von JSON in Bash können auf der offiziellen "jq" Website (https://stedolan.github.io/jq/) gefunden werden. Auch auf der Learning-Plattform "Codecademy" (https://www.codecademy.com/learn/learn-json) gibt es interaktive Übungen zum Thema JSON.