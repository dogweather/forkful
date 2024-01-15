---
title:                "Arbeiten mit YAML"
html_title:           "C: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## Warum

Wenn du das Programmieren mit C beherrschst, hast du vielleicht schon von YAML gehört. Aber wofür braucht man das überhaupt? Nun, YAML ist eine außerordentlich nützliche Datenformatierungssprache, die dabei hilft, komplexe Daten leicht lesbar und übersichtlich zu machen. Mit C und YAML kannst du komplexe Datenstrukturen erstellen und verwalten, was dir dabei hilft, effizientere und professionellere Programme zu schreiben.

## How To

Um mit YAML in C zu arbeiten, musst du zunächst die Bibliothek "libyaml" in dein Programm einbinden. Diese Bibliothek ermöglicht es, YAML-Dateien zu lesen und zu schreiben. Anschließend kannst du mit den YAML-Daten arbeiten, indem du sie in eine geeignete Datenstruktur einliest, beispielsweise in ein Array oder eine Struktur. Hier ein Beispielcode:

```C
#include <stdio.h>
#include <yaml.h>

int main()
{
    // Öffne YAML-Datei
    FILE *yaml_file = fopen("beispiel.yaml", "r");

    // Initialisiere den Parser
    yaml_parser_t parser;
    yaml_parser_initialize(&parser);

    // Wenn das Einlesen erfolgreich war...
    if (yaml_parser_load(&parser, yaml_file))
    {
        yaml_event_t event;
        int done = 0;

        // Gehe durch jedes Element und gib es aus
        while (!done)
        {
            // Lese das nächste Event
            if (!yaml_parser_parse(&parser, &event))
                break;

            // Wenn das Event ein Mapping ist...
            if (event.type == YAML_MAPPING_START_EVENT)
            {
                fprintf(stdout, "Eintrag gefunden:\n");
            }
            else if (event.type == YAML_SCALAR_EVENT)
            {
                // Gib den Eintrag und seinen Wert aus
                fprintf(stdout, "%s: %s\n", event.data.scalar.value, event.data.scalar.value);
            }
            done = (event.type == YAML_STREAM_END_EVENT);
            // Event löschen
            yaml_event_delete(&event);
        }

        // Parser beenden
        yaml_parser_delete(&parser);
    }

    // Schließe die Datei
    fclose(fp);

    return 0;
}
```

Der Code liest eine YAML-Datei mit dem Namen "beispiel.yaml" ein und gibt dann alle Einträge und ihre Werte aus. Natürlich kannst du diesen Code anpassen und erweitern, um damit Datenstrukturen zu erstellen, zu ändern oder zu speichern.

## Deep Dive

Jetzt wo du den Grundsatz von YAML in C kennengelernt hast, kannst du weiter forschen und noch mehr lernen. Zum Beispiel kannst du dir die Dokumentation zur "libyaml"-Bibliothek ansehen, um mehr über die verfügbaren Funktionen zu erfahren. Du kannst auch damit experimentieren, wie du komplexe Datenstrukturen mit YAML und C erstellen und verwalten kannst. Die Möglichkeiten sind endlos, also zögere nicht, noch tiefer in dieses spannende Thema einzutauchen.

## Siehe auch

- [offizielle YAML-Website](http://yaml.org/)
- [libyaml-Dokumentation](https://pyyaml.org/wiki/LibYAML)
- [Beispiele für YAML-Coding in verschiedenen Programmiersprachen](https://github.com/jeremygiberson/yaml-cpp/wiki/Tutorials)

Danke fürs Lesen und viel Spaß beim Coden mit YAML in C!