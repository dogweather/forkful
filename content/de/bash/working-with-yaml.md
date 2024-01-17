---
title:                "Arbeiten mit YAML"
html_title:           "Bash: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## Was & Warum?

Bash (Bourne-Again Shell) ist eine beliebte interaktive Shell und Skriptsprache, die in verschiedenen Betriebssystemen wie Linux, macOS und anderen eingesetzt wird. YAML ist eine datenorientierte Dateiformatierungssprache, die häufig in Kombination mit Bash verwendet wird, um intuitiv lesbaren Daten- und Konfigurationsdateien zu erstellen. Programmierer nutzen YAML, um ihre Skripte und Programme effizient und funktional zu gestalten.

## Wie geht das?

Um mit YAML in Bash zu arbeiten, muss zuerst ein YAML-Parser installiert werden. Ein Beispiel dafür ist das Programm "yaml2json". Hier ist der Code, um YAML in ein JSON-Format umzuwandeln:

```Bash
$ yaml2json example.yaml
```

Das Ergebnis sieht dann etwa so aus:

```Bash
{
   "name": "John Doe",
   "age": 30,
   "favorite_foods": ["pizza", "sushi", "chocolate"]
}
```

## Tiefere Einblicke

YAML wurde ursprünglich von einem Debian-Entwickler als einfacheres und übersichtlicheres Alternativ zu XML entwickelt. Neben der Verwendung in Bash ist YAML auch in anderen Programmiersprachen wie Python, Ruby und Java weitverbreitet. YAML Syntax ist auf eine einfache und lesbare Darstellung von Daten ausgelegt, wodurch sie für Programmierer und Nicht-Programmierer gleichermaßen zugänglich ist.

## Siehe auch

Wenn du mehr über die Verwendung von YAML in Bash erfahren möchtest, empfehlen wir dir, die offizielle YAML-Website und die Dokumentation von Bash zu besuchen. Du findest auch weitere hilfreiche Ressourcen wie Tutorials und Beispiele auf GitHub.