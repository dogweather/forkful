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

## Warum 
Willst du deine Daten in einem menschenlesbaren Format speichern? YAML ist eine großartige Option für die Speicherung von strukturierten Daten. Es ist einfach zu lesen und zu schreiben, was es ideal für die Verwendung in kleinen Projekten macht.

## Wie Geht's
Um mit YAML in C++ zu arbeiten, müssen Sie zunächst die Bibliothek "yaml-cpp" installieren. Dann können Sie mit dem Speichern und Lesen von YAML-Daten in Ihrem Code beginnen.

```C++
// Beispiel für das Schreiben von YAML-Daten in eine Datei
YAML::Emitter emitter;
emitter << YAML::BeginMap;
emitter << YAML::Key << "Name" << YAML::Value << "Max Mustermann";
emitter << YAML::Key << "Alter" << YAML::Value << 25;
emitter << YAML::EndMap;

// Beispiel für das Lesen von YAML-Daten aus einer Datei
YAML::Node data = YAML::LoadFile("daten.yaml");
std::string name = data["Name"].as<std::string>();
int alter = data["Alter"].as<int>();

std::cout << "Name: " << name << std::endl;
std::cout << "Alter: " << alter << std::endl;
```

## Tiefergehende Informationen
YAML (eine Abkürzung für "YAML Ain't Markup Language") ist eine einfache und menschenlesbare Sprache zum Speichern von Daten. Es ähnelt stark dem beliebten JSON-Format, aber YAML ist noch einfacher zu lesen und zu schreiben. Es verwendet Einrückungen und Leerzeichen, um die Struktur von Daten darzustellen, was es besonders lesbar macht.

Eine weitere interessante Funktion von YAML ist die Möglichkeit, Anker und Alias zu verwenden, um Duplikate von Daten zu vermeiden. Dies ist besonders nützlich, wenn Sie komplexe Datenstrukturen mit wiederkehrenden Elementen haben.

Kommentare sind ebenfalls möglich, indem Sie die Raute (#) verwenden. Dadurch können Sie Ihre YAML-Dateien annotieren und erklären.

## Siehe Auch
- [yaml-cpp GitHub Repository](https://github.com/jbeder/yaml-cpp)
- [YAML 1.2 Spezifikation](https://yaml.org/spec/1.2/spec.html) 
- [YAML Tutorial von TutorialsPoint](https://www.tutorialspoint.com/yaml/)