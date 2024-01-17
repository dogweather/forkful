---
title:                "Arbeiten mit YAML"
html_title:           "C#: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Was & Warum?
YAML ist eine einfache, menschenlesbare Datenstrukturierungssprache. Programmer nutzen sie, um Daten in einem klaren Textformat zu speichern und zu lesen. Es ist einfach zu schreiben und zu verstehen, und wird häufig in Konfigurations- und Metadaten verwendet.

## Wie geht das?
Zum Lesen und Schreiben von YAML-Dateien in C# benötigt man eine Bibliothek wie z.B. YamlDotNet. Hier ist ein Beispielcode:

```C#
// Lesen einer YAML-Datei
var input = new StreamReader("dokument.yml");
var deserializer = new DeserializerBuilder().Build();
var doc = deserializer.Deserialize(input);

// Schreiben einer YAML-Datei
var data = new Dictionary<string, object>
{
    { "name", "Max Mustermann" },
    { "alter", 25 },
    { "hobbys", new string[] { "Lesen", "Radfahren", "Musik" } }
};
var serializer = new SerializerBuilder().Build();
var output = serializer.Serialize(data);
File.WriteAllText("meine_daten.yml", output);
```

## Tief einsteigen
YAML wurde in den frühen 2000er Jahren entwickelt und ist inspiriert von Programmiersprachen wie Perl, Python und C. Es bietet eine flexible und leserliche Alternative zu XML und JSON. Alternativen zu YAML sind beispielsweise Toml oder JSON5. 
Die Implementierung von YAML in C# bietet verschiedene Möglichkeiten zur Serialisierung und Deserialisierung von Daten, abhängig von den jeweiligen Anforderungen.

## Siehe auch
- Offizielle YAML-Website: https://yaml.org/
- Dokumentation für YamlDotNet: https://github.com/aaubry/YamlDotNet/wiki
- Einsteiger-Tutorial für YAML: https://rollout.io/blog/yaml-tutorial-everything-you-need-get-started/