---
title:                "Arbeiten mit JSON"
html_title:           "PHP: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/working-with-json.md"
---

{{< edit_this_page >}}

# Was & Warum?

JSON, kurz für "JavaScript Object Notation", ist ein Datenformat, das häufig von Programmierern verwendet wird, um strukturierte Daten auszutauschen. Es ist im Wesentlichen eine einfache Textdarstellung von Objekten und Arrays, die von verschiedenen Programmiersprachen unterstützt wird. Das formatierte und lesbarer menschlicher Programmierstil macht es zu einer beliebten Wahl für den Austausch von Daten zwischen Servern und Clients.

## Wie man:

Der einfachste Weg, mit JSON in PHP zu arbeiten, ist durch die Verwendung von integrierten Funktionen wie `json_encode()` und `json_decode()`. Zum Beispiel:

```PHP
// Ein Array erstellen
$array = ["Name" => "Peter", "Alter" => 25, "Hobby" => "Fußball"];

// Datensatz in JSON konvertieren
$json = json_encode($array);

// JSON wieder in ein Array umwandeln
$newArray = json_decode($json);

// Ausgabe des neuen Arrays lesen
var_dump($newArray);
```

Die Ausgabe sollte folgendes enthalten:

```
array(3) {
  ["Name"]=>
  string(5) "Peter"
  ["Alter"]=>
  int(25)
  ["Hobby"]=>
  string(8) "Fußball"
}
```

## Tiefer tauchen:

JSON wurde erstmals 2001 von Douglas Crockford eingeführt und hat sich seitdem zu einem der am häufigsten verwendeten Datenformate entwickelt. Es hat auch Alternativen wie XML und YAML, aber JSON ist aufgrund seiner einfachen Struktur und der Unterstützung durch viele Programmiersprachen sehr beliebt.

Um die Leistung beim Codieren und Decodieren großer JSON-Datenmengen zu verbessern, gibt es auch Erweiterungen wie JSON-PHP und Jansson. Diese können verwendet werden, um Daten in einem effizienteren Speicherformat zu verarbeiten und so die Verarbeitungsgeschwindigkeit zu erhöhen.

## Siehe auch:

- Offizielle PHP-Dokumentation zu JSON: https://www.php.net/manual/en/book.json.php
- JSON.org: https://www.json.org/
- Douglas Crockfords Seite zu JSON: https://www.crockford.com/mckeeman.html