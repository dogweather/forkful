---
title:                "PHP: Die Arbeit mit yaml."
simple_title:         "Die Arbeit mit yaml."
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/working-with-yaml.md"
---

{{< edit_this_page >}}

# Warum YAML? 

YAML ist eine einfache, leicht verständliche und menschenlesbare Sprache für die Datenformatierung. Sie wird oft verwendet, um Konfigurationsdateien zu erstellen, die von verschiedenen Anwendungen gelesen werden können. YAML ist besonders nützlich für Entwickler, die eine flexiblere und übersichtlichere Alternative zu anderen Dateiformaten wie JSON suchen.

# Wie man YAML verwendet

Um YAML in PHP zu verwenden, müssen Sie zunächst die YAML-Erweiterung installieren. Sobald dies erledigt ist, können Sie die Funktion `yaml_parse()` verwenden, um eine YAML-Datei in eine PHP-Array umzuwandeln. Sie können auch die Funktion `yaml_emit()` verwenden, um ein PHP-Array in eine YAML-Datei zu konvertieren.

Hier ist ein Beispiel, wie man eine YAML-Datei in PHP liest und ausgibt:

```PHP
<?php
$yaml = file_get_contents("config.yml");
$config = yaml_parse($yaml);
echo $config["database"]["host"]; // gibt den Wert "localhost" aus
```

Und hier ist ein Beispiel, wie man ein PHP-Array in eine YAML-Datei schreibt:

```PHP
<?php
$config = array(
    "database" => array(
        "host" => "localhost",
        "username" => "root",
        "password" => "root"
    )
);
$yaml = yaml_emit($config);
file_put_contents("config.yml", $yaml);
```

Die Ausgabe der beiden Beispiele ist die folgende YAML-Datei:

```yaml
database:
  host: localhost
  username: root
  password: root
```

# Tiefer eintauchen in YAML

YAML bietet eine Vielzahl von Funktionen, die es ermöglichen, komplexe Datenstrukturen zu definieren und zu speichern. So können beispielsweise Listen und verschachtelte Arrays erstellt werden. Auch das Einbinden anderer YAML-Dateien und das Definieren von Anker und Referenzen sind möglich.

Ein weiterer Vorteil von YAML ist, dass es mehrere Arten der Strukturierung unterstützt. So können beispielsweise Daten als Listen, Hash-Maps oder in objektorientierter Notation gespeichert werden. Dies macht YAML zu einer flexiblen Option für verschiedene Anwendungsfälle.

# Siehe auch

- Offizielle YAML-Dokumentation (http://yaml.org)
- PHP Class - Spyc (https://github.com/mustangostang/spyc)
- Symfony YAML Component (https://symfony.com/doc/current/components/yaml.html)