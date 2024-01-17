---
title:                "Arbeiten mit YAML"
html_title:           "PHP: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## Was & Warum?

YAML steht für "YAML Ain't Markup Language" und ist eine einfache, lesenswerte Datenformatierungssprache. Es wird von Programmierern verwendet, um strukturierte Daten zu speichern und zu übermitteln. YAML ist leicht zu verstehen und somit eine beliebte Alternative zu komplexeren Datenformaten wie XML.

## Wie geht's?

Mit PHP kannst du ganz einfach YAML-Daten lesen, schreiben und bearbeiten. Hier ist ein Beispiel, wie du eine YAML-Datei öffnen und den Inhalt in ein Array speichern kannst:

```PHP
$yaml = file_get_contents('meine_daten.yml');
$array = yaml_parse($yaml);
echo $array['name']; // gibt den Wert des Schlüssels "name" aus
```

Und so könntest du Änderungen an der Datenstruktur vornehmen und sie wieder als YAML-Datei speichern:

```PHP
$array['hobbies'][] = 'Fotografie'; // fügt ein neues Hobby hinzu
$yaml = yaml_emit($array);
file_put_contents('meine_daten.yml', $yaml);
```

Das war's schon! Mit diesen einfachen Funktionen kannst du problemlos mit YAML-Daten in PHP arbeiten.

## Tief eintauchen

YAML wurde ursprünglich von Clark Evans als Teil des Projekts "Lightweight Data Interchange Format" entwickelt und 2001 veröffentlicht. Seitdem hat es immer mehr an Beliebtheit gewonnen und wird mittlerweile von vielen Programmiersprachen unterstützt.

Eine andere Möglichkeit, strukturierte Daten in PHP zu speichern, wäre JSON (JavaScript Object Notation). Im Vergleich zu YAML ist JSON jedoch weniger lesbar und benutzerfreundlich. YAML ist also die bessere Wahl, wenn es darum geht, Daten für Menschen lesbar zu machen.

In PHP wird YAML über die Bibliothek "LibYAML" implementiert, die in der Standardbibliothek der Sprache enthalten ist. Damit kannst du sicher sein, dass YAML-Dateien in allen Umgebungen gleich funktionieren.

## Siehe auch

- [Offizielle YAML-Website](https://yaml.org/)
- [PHP-Dokumentation zu YAML](https://www.php.net/manual/en/function.yaml-parse.php)
- [Einführung in die YAML-Syntax](https://learnxinyminutes.com/docs/yaml/)