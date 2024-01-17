---
title:                "Arbeiten mit Yaml"
html_title:           "C: Arbeiten mit Yaml"
simple_title:         "Arbeiten mit Yaml"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/working-with-yaml.md"
---

{{< edit_this_page >}}

Was ist YAML und warum nutzen Programmierer es?

YAML steht für "YAML Ain't Markup Language" und ist eine einfache, aber leistungsstarke Sprache zur Strukturierung von Daten. Als Programmierer nutzen wir YAML, um komplexe Datenstrukturen in einer lesbareren und wartbareren Weise zu organisieren.

Wie geht man vor:

```C
int main() {
  // Beispiel für ein YAML-Dokument
  char yaml[20] = "name: Max";
  char output[20];

  // Abrufen und Ausgeben des Namen
  strcpy(output, yaml_parse(yaml, "name"));
  printf("%s\n", output);
  return 0;
}
```

Dieses Beispiel zeigt, wie man einen einfachen Namen aus einem YAML-Dokument extrahieren kann. Die verwendete Funktion "yaml_parse" ist Teil der meisten YAML-Bibliotheken und ermöglicht es uns, Daten basierend auf Schlüsselwörtern abzurufen.

Tiefergehende Informationen:

YAML wurde erstmals 2001 veröffentlicht und war eines der ersten Datenformate, das auf menschenlesbarer Syntax basierte. Als Alternative zu komplexeren Datenformaten wie XML wurde YAML schnell beliebt unter Entwicklern. Es ist auch wichtig zu beachten, dass YAML nicht nur in der Programmierung, sondern auch in der Konfiguration von Systemen weit verbreitet ist.

Siehe auch:

- Offizielle YAML-Website: https://yaml.org/
- YAML-Spezifikation: https://yaml.org/spec/
- Alternative Datenformate: JSON, XML