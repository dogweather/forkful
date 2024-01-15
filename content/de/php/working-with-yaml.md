---
title:                "Arbeiten mit yaml"
html_title:           "PHP: Arbeiten mit yaml"
simple_title:         "Arbeiten mit yaml"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## Warum
Warum solltest du dich mit YAML beschäftigen? Nun, YAML ist eine einfache, menschenlesbare Sprache, die verwendet wird, um Daten in einer strukturierten Form zu speichern. Es ist besonders nützlich für Entwickler, die mit komplexen Datenstrukturen arbeiten, wie z.B. Konfigurationsdateien.

## Wie funktioniert es
Um mit YAML in PHP zu arbeiten, musst du zunächst die YAML-Erweiterung installieren. Dies kann entweder manuell oder über einen Paketmanager wie Composer erfolgen. Sobald die Erweiterung installiert ist, kannst du einfache YAML-Dokumente wie folgt parsen:

```PHP
$data = yaml_parse('Name: Max
Alter: 30
Hobbies:
    - Laufen
    - Lesen');
```

Die Ausgabe wird dann so aussehen:

```PHP
array(3) {
  ["Name"]=>
  string(3) "Max"
  ["Alter"]=>
  int(30)
  ["Hobbies"]=>
  array(2) {
    [0]=>
    string(6) "Laufen"
    [1]=>
    string(6) "Lesen"
  }
}
```

Wenn du ein YAML-Dokument in eine Datei schreiben möchtest, kannst du dies ebenfalls mit der Funktion `yaml_emit` tun:

```PHP
$data = ['Name' => 'Max', 'Alter' => 30, 'Hobbies' => ['Laufen', 'Lesen']];
$yaml = yaml_emit($data);
file_put_contents('meine_daten.yml', $yaml);
```

## Tief tauchen
Neben den grundlegenden Funktionen gibt es eine Vielzahl von erweiterten Funktionen und Optionen für die Arbeit mit YAML in PHP. Du kannst beispielsweise benutzerdefinierte Typen erstellen, um deine YAML-Dokumente noch flexibler zu gestalten. Außerdem bietet die YAML-Erweiterung eine integrierte Validierung, um sicherzustellen, dass deine Daten fehlerfrei sind.

Ein wichtiger Aspekt beim Arbeiten mit YAML ist auch die Performance. Durch die Verwendung der in C geschriebenen YAML-Erweiterung anstelle einer reinen PHP-Implementierung wird die Verarbeitung von YAML-Dokumenten deutlich beschleunigt.

## Siehe auch
- [Offizielle Dokumentation der YAML-Erweiterung](http://www.php.net/manual/de/book.yaml.php)
- [YAML-Spezifikation](https://yaml.org/spec/1.2/spec.html)
- [Symfony YAML-Komponente](https://symfony.com/doc/current/components/yaml.html)