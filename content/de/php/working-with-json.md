---
title:                "Arbeiten mit JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"

category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/working-with-json.md"
---

{{< edit_this_page >}}

## Was & Warum?
JSON ist ein Format zum Austausch von Daten zwischen verschiedenen Systemen. Programmierer verwenden es wegen seiner Einfachheit und Sprachunabhängigkeit, um Daten leicht zwischen Server und Client zu übertragen.

## How to:
Mit PHP ist das Handling von JSON mit den Funktionen `json_encode()` und `json_decode()` ein Kinderspiel. Hier ein einfaches Beispiel:

```PHP
<?php
// Ein PHP-Array erstellen
$daten = array("Name" => "Max", "Alter" => 25, "Stadt" => "Berlin");

// Array in JSON umwandeln
$json = json_encode($daten);
echo $json; // {"Name":"Max","Alter":25,"Stadt":"Berlin"}

// JSON zurück in ein PHP-Objekt konvertieren
$objekt = json_decode($json);
echo $objekt->Name; // Max
?>
```

## Deep Dive
JSON (JavaScript Object Notation) wurde Anfang der 2000er Jahre populär und ist im Vergleich zu XML viel leichtgewichtiger. Alternativen wie XML oder YAML sind teils komplexer. In PHP wird JSON als Objekt oder Array interpretiert, wobei `json_decode()` einen zweiten Parameter hat, um zwischen beiden zu wählen. Beachten, dass `json_encode()` nicht gut mit nicht UTF-8 kodierten Zeichen umgeht und Fehlerbehandlung nötig ist, wenn JSON fehlerhaft oder nicht wohlgeformt ist.

## See Also
- [PHP Manual on JSON](https://www.php.net/manual/en/book.json.php)
- [W3Schools PHP JSON Tutorial](https://www.w3schools.com/php/php_ref_json.asp)
- [JSON Introduction on MDN](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/JSON)
