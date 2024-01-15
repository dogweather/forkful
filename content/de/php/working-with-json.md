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

## Warum

Wenn du regelmäßig mit Daten in deinen PHP-Projekten arbeitest, bist du wahrscheinlich schon auf das Format JSON (JavaScript Object Notation) gestoßen. JSON ist ein leicht verständliches und platzsparendes Format, das perfekt für die Übertragung von Daten zwischen Client und Server geeignet ist. In diesem Artikel erklären wir dir, warum es sich lohnt, sich mit JSON in PHP zu beschäftigen.

## Wie geht das?

Die Verarbeitung von JSON in PHP ist sehr einfach und intuitiv. Wir verwenden dazu die Standardfunktionen `json_encode()` und `json_decode()`. `json_encode()` wandelt eine PHP-Variable in das JSON-Format um, während `json_decode()` das JSON in eine PHP-Variable umwandelt.

```PHP
// Beispiel JSON-Daten
$json_data = '{"name": "Max Mustermann", "age": 25, "hobbies": ["lesen", "Musik hören"]}';

// Umwandeln von JSON in ein Array
$array = json_decode($json_data);

// Ausgabe des Namens
echo "Name: " . $array["name"]; // Ausgabe: Name: Max Mustermann

// Hinzufügen eines neuen Hobbys
$array["hobbies"][] = "Fotografieren";

// Umwandeln von Array in JSON
$json_data = json_encode($array);
```

## Tiefer Einblick

Neben dem Umwandeln von Daten bietet PHP auch die Möglichkeit, direkt mit JSON-Dateien zu arbeiten. Mit der Funktion `file_get_contents()` können wir den Inhalt einer JSON-Datei in eine Variable laden und sie mit `json_decode()` in ein PHP-Array umwandeln.

Eine besondere Stärke von JSON in PHP ist die Unterstützung von assoziativen Arrays. Durch die Verwendung von `json_decode()` mit dem zweiten Parameter `true` können wir JSON-Daten direkt in assoziative Arrays umwandeln.

```PHP
// Beispiel JSON-Datei
{
    "name": "Lisa Müller",
    "age": 30,
    "hobbies": ["Malen", "Reisen"]
}

// Laden der JSON-Datei
$json_data = file_get_contents("data.json");

// Umwandeln in assoziatives Array
$array = json_decode($json_data, true);

// Ausgabe des Alters
echo "Alter: " . $array["age"]; // Ausgabe: Alter: 30

// Hinzufügen eines neuen Hobbys
$array["hobbies"][] = "Tanzen";

// Umwandeln von Array in JSON
$json_data = json_encode($array);
```

Zusätzlich bietet PHP auch die Möglichkeit, die Struktur von JSON-Daten zu validieren, um sicherzustellen, dass sie den Anforderungen entspricht. Hierfür können wir die Funktion `json_last_error()` verwenden, die uns den Fehlercode der letzten JSON-Aktion zurückgibt.

## Siehe auch

- [Offizielle PHP-Dokumentation zu JSON](https://www.php.net/manual/de/book.json.php)
- [JSON - Eine Einführung von MDN Web Docs](https://developer.mozilla.org/de/docs/Learn/JavaScript/Objects/JSON)
- [JSON in PHP von jQuery Rain](https://www.jqueryrain.com/2017/09/json-php-jQuery/)