---
title:                "Arbeiten mit YAML"
html_title:           "C++: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Was ist YAML und warum nutzen Programmierer es?

YAML ist eine einfache, menschenlesbare Datenformatierungssprache, die häufig von Programmierern verwendet wird, um Daten in einer kompakten und leicht verständlichen Art und Weise zu speichern und zu übertragen. YAML wird vor allem in der Konfiguration von Software oder beim Übertragen von Daten zwischen verschiedenen Anwendungen genutzt.

## Anleitung:

Die Verwendung von YAML in C++ ist recht einfach. Im Folgenden werden zwei Beispiele gezeigt, wie man Daten in YAML schreibt und liest. Beachte, dass für die folgenden Code-Beispiele eine YAML-Bibliothek wie yaml-cpp verwendet werden muss.

### Beispiel 1: Schreiben von Daten in YAML

```C++
YAML::Node data; // Erstellen eines leeren YAML-Nodes

// Hinzufügen von Daten zu dem Node
data["Name"] = "Max";
data["Alter"] = 25;
data["Hobbies"] = { "Lesen", "Musik", "Laufen" };

// Schreiben des Nodes in eine Datei
std::ofstream datei("daten.yml");
datei << data; 
```

### Beispiel 2: Lesen von Daten aus YAML

```C++
// Einlesen von YAML-Daten aus einer Datei
YAML::Node data = YAML::LoadFile("daten.yml");

// Zugriff auf einzelne Daten
std::string name = data["Name"].as<std::string>();
int alter = data["Alter"].as<int>();

// Iterieren über eine Liste von Daten
for (std::string hobby : data["Hobbies"]) {
    std::cout << hobby << " ";
}
```

### Ausgabe:

```C++
Name: Max
Alter: 25
Hobbies: Lesen Musik Laufen
```

## Tiefentauchen:

### Historischer Hintergrund:

YAML wurde im Jahr 2001 vom Programmierer Clark Evans erfunden. Es wurde als Alternative zu anderen komplexeren Datenformaten wie XML entwickelt und hat sich seitdem zu einem beliebten Format bei Programmierern entwickelt.

### Alternativen:

Obwohl YAML von vielen Programmierern bevorzugt wird, gibt es auch andere Datenformate, die ähnliche oder sogar bessere Funktionen bieten. Dazu gehören JSON, TOML und INI-Dateien. Es ist wichtig zu prüfen, welches Format am besten zu den Anforderungen des jeweiligen Projekts passt.

### Implementierungsdetails:

YAML wird durch Tabellen und Listen dargestellt, die durch Einrückungen voneinander getrennt sind. Es ist daher wichtig, beim Schreiben von YAML-Dateien die Einrückungen korrekt zu setzen, da dies die Struktur und Lesbarkeit der Datei beeinflusst.

## Siehe auch:

- yaml-cpp Bibliothek: https://github.com/jbeder/yaml-cpp
- YAML-Spezifikation: https://yaml.org/spec/1.2/spec.html
- Vergleich verschiedener Datenformate: https://github.com/ztellman/data-format-comparison