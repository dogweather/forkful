---
title:                "PHP: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/working-with-json.md"
---

{{< edit_this_page >}}

# Warum mit JSON arbeiten?

JSON ist eine gängige Datenformat, das in der Webentwicklung weit verbreitet ist. Es ermöglicht es, Daten in einem leserlichen und kompakten Format zu speichern, wodurch es optimal für den Austausch von Informationen zwischen Webanwendungen geeignet ist. In diesem Blogbeitrag erfahren Sie, warum es sich lohnt, mit JSON zu arbeiten.

## Wie man mit JSON arbeitet

Um mit JSON in PHP zu arbeiten, müssen Sie zunächst sicherstellen, dass die `json`-Erweiterung in Ihrem PHP installiert ist. In den meisten Fällen ist dies bereits standardmäßig vorhanden. Dann können Sie mit der Funktion `json_encode()` eine PHP-Variable in ein JSON-Format umwandeln, oder mit `json_decode()` ein JSON-Objekt in eine PHP-Variable umwandeln. Hier ist ein Beispiel:

```PHP
// Eine PHP-Variable
$person = array(
    'name' => 'Max Mustermann',
    'age' => 30,
    'hobbies' => array('Kochen', 'Sport', 'Reisen')
);

// Konvertieren in JSON-Format
$json = json_encode($person);

echo $json;
// Ausgabe: {"name": "Max Mustermann", "age": 30, "hobbies": ["Kochen", "Sport", "Reisen"]}

// Konvertieren von JSON in PHP-Variable
$person_obj = json_decode($json);

echo $person_obj->name;
// Ausgabe: Max Mustermann
```

Sie können auch verschachtelte JSON-Objekte oder Arrays erstellen, indem Sie einfach die entsprechenden Variablen in das `json_encode()` oder `json_decode()` setzen.

## Tiefergehende Informationen über JSON

Ein wichtiger Aspekt beim Arbeiten mit JSON ist das Validieren oder Überprüfen von JSON-Daten. Hierfür bietet PHP die Funktion `json_last_error()` an, die den Fehlercode der letzten JSON-Operation zurückgibt. Dies kann hilfreich sein, um Fehler in den Daten zu finden und zu beheben.

Eine weitere nützliche Funktion ist `json_encode()` mit dem optionalen Parameter `JSON_PRETTY_PRINT`. Dies ermöglicht es, das JSON-Format mit Einzügen und Zeilenumbrüchen lesbarer zu machen, was besonders hilfreich ist, wenn Sie mit komplexen Daten arbeiten.

Es ist auch wichtig zu beachten, dass nicht alle JSON-Objekte oder Arrays in PHP-Variablen konvertiert werden können. Einige Datentypen, wie z.B. boolesche Werte oder NULL, können nicht direkt in JSON umgewandelt werden. Eine mögliche Lösung hierfür ist die Verwendung eines benutzerdefinierten Serializers, der diese Daten in geeignete Werte konvertiert.

# Siehe auch

- [PHP-Dokumentation zu JSON](https://www.php.net/manual/en/book.json.php)
- [Online JSON-Validator](https://jsonlint.com/)
- [How to Use JSON Data with PHP or JavaScript](https://www.sitepoint.com/use-json-data-php-javascript/)