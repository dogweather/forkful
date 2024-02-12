---
title:                "Arbeiten mit YAML"
aliases:
- /de/c/working-with-yaml.md
date:                  2024-02-03T18:13:12.925228-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

YAML, das für "YAML Ain't Markup Language" steht, ist ein für Menschen lesbaren Daten-Serialisierungsstandard, der für alle möglichen Anwendungen genutzt werden kann, von Konfigurationsdateien bis hin zur Datenspeicherung. Programmierer arbeiten oft mit YAML, wenn sie ein leicht zu lesendes und zu schreibendes Format für Konfigurationsdateien oder den Datenaustausch zwischen Sprachen und Systemen benötigen.

## Wie geht das:

Die Arbeit mit YAML in C erfordert eine Bibliothek, da die Standard-C-Bibliothek keine direkte Unterstützung für YAML-Parser oder Serialisierung bietet. Eine der beliebtesten YAML-Bibliotheken für C ist `libyaml`, die sowohl niedrig- als auch hochstufige Schnittstellen für das Parsen und Ausgeben von YAML bereitstellt. Im Folgenden finden Sie ein Beispiel, wie eine einfache YAML-Datei mit `libyaml` geparst wird:

**Zuerst** müssen Sie die `libyaml` Bibliothek installieren. Wenn Sie auf einem Unix-ähnlichen System sind, können Sie sie normalerweise über Ihren Paketmanager installieren. Zum Beispiel auf Ubuntu:

```bash
sudo apt-get install libyaml-dev
```

**Als Nächstes** betrachten Sie eine einfache YAML-Datei namens `config.yaml`:

```yaml
name: John Doe
age: 29
married: false
```

**Hier ist** ein grundlegendes Beispiel, wie man diese YAML-Datei in C parst:

```c
#include <yaml.h>
#include <stdio.h>
#include <stdlib.h>

void process_yaml_file(const char *filename) {
    FILE *fh = fopen(filename, "rb");
    yaml_parser_t parser;
    yaml_event_t event;

    if (!yaml_parser_initialize(&parser))
        fputs("YAML-Parser-Initialisierung fehlgeschlagen!\n", stderr);

    if (fh == NULL)
        fputs("Datei konnte nicht geöffnet werden!\n", stderr);

    yaml_parser_set_input_file(&parser, fh);

    while (1) {
        if (!yaml_parser_parse(&parser, &event))
            break;

        if (event.type == YAML_SCALAR_EVENT) {
            printf("Wert: %s\n", event.data.scalar.value);
        }

        if (event.type == YAML_STREAM_END_EVENT)
            break;

        yaml_event_delete(&event);
    }

    yaml_parser_delete(&parser);
    fclose(fh);
}

int main() {
    process_yaml_file("config.yaml");
    return 0;
}
```

Dieses einfache Programm öffnet eine YAML-Datei, initialisiert den YAML-Parser und liest die Datei und druckt die Skalarwerte (in diesem Beispiel die Felder unserer einfachen YAML). Beachten Sie, dass die Fehlerüberprüfung in diesem einfachen Beispiel minimal ist und in Produktionscode robuster sein sollte.

Die Ausführung des Programms mit unserer `config.yaml` gibt aus:

```plaintext
Wert: John Doe
Wert: 29
Wert: false
```

## Vertiefung

YAML wurde erstmals im Jahr 2001 veröffentlicht und so konzipiert, dass es im Vergleich zu anderen Daten-Serialisierungsformaten wie XML oder JSON lesbarer und benutzerfreundlicher ist, wobei es von mehreren Sprachen wie C, Perl und Python für seine Designphilosophie entlehnt. Trotz seiner Vorteile in Bezug auf Lesbarkeit und einfache menschliche Modifizierung kann YAML programmatisch aufgrund seiner Abhängigkeit von Einrückungen und seinem umfangreichen Funktionsset, einschließlich Referenzen und benutzerdefinierten Typen, komplex zu parsen sein.

Während `libyaml` robusten, niedrigstufigen Zugriff auf das Parsen und Ausgeben von YAML in C bietet, kann es für einfache Aufgaben aufgrund seiner umständlichen API mühsam sein. Aus diesen Gründen bevorzugen einige Programmierer die Verwendung von höherstufigen Bibliotheken oder sogar anderen Daten-Serialisierungsformaten wie JSON beim Arbeiten in C, insbesondere wenn performantes Parsen mit minimalem Code-Overhead Priorität hat. Dennoch bleibt YAML eine beliebte Wahl für Konfigurationsdateien und Situationen, in denen Lesbarkeit für Menschen von größter Bedeutung ist. Alternativen wie TinyYAML oder das Einbetten eines höherstufigen Interpreters (z.B. Einbetten von Python oder Lua) könnten für spezifische Anwendungen mehr Bequemlichkeit bieten und einen Ausgleich zwischen Benutzerfreundlichkeit und Leistungsanforderungen schaffen.
