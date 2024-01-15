---
title:                "Arbeiten mit YAML"
html_title:           "Gleam: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## Warum 

In der Welt der Softwareentwicklung gibt es oft Situationen, in denen wir Daten speichern und verarbeiten müssen. Egal ob es sich um Konfigurationsdateien, Datenbanken oder APIs handelt, YAML ist eine beliebte Wahl als Datenformat aufgrund seiner Lesbarkeit und Einfachheit. In diesem Artikel werden wir untersuchen, wie Sie mit YAML in Gleam arbeiten können. 

## Wie geht das

Um mit YAML in Gleam zu arbeiten, benötigen wir die bibliothek `gleam-yaml`, die wir zum `rebar.config` unseres Projekts hinzufügen. Als nächstes importieren wir die Funktionen aus der `gleam-yaml` Bibliothek und bereiten unser Beispiel vor, indem wir ein YAML-Dokument erstellen. 

```Gleam 
import gleam-yaml 

let my_yaml = 
    """ 
    name: Gleam 
    category: Programming Language 
    website: https://gleam.run/ 
    """ 
``` 

Um das YAML-Dokument zu analysieren und die Daten zu extrahieren, verwenden wir die Funktion `Yaml.parse`. Dies gibt uns ein Gleam-Typ `Result(Yaml.Error, Yaml.Value)`, der entweder eine Fehlermeldung oder die extrahierten Daten enthält. 

```Gleam 
let parsed_yaml = Yaml.parse(my_yaml) 
``` 

Um auf die Daten zuzugreifen, können wir das `match`-Ausdruck verwenden, um auf die beiden möglichen Fälle, also Fehler oder Erfolg, zu reagieren. 

```Gleam 
match Yaml.parse(my_yaml) { 
    Ok(yaml_value) -> 
        // Hier können wir auf die Daten zugreifen 
        let name = yaml_value |. Yaml.Key("name") |. Yaml.String 
    Err(error) -> 
        // Hier können wir auf den Fehler reagieren 
        Debug.todo(error) 
} 
``` 

## Tief eintauchen 

Wir haben bereits gesehen, wie wir YAML-Daten in Gleam analysieren können, aber es gibt noch viel mehr zu entdecken. 
Als nächstes können wir beispielsweise die Daten in ein anderes Format konvertieren, indem wir die Funktion `Yaml.encode` verwenden. Wir können auch komplexere Daten wie Arrays und verschachtelte Objekte analysieren. Um mehr über die verfügbaren Funktionen und Optionen zu erfahren, empfehlen wir Ihnen, die Dokumentation der `gleam-yaml` Bibliothek zu lesen. 

## Siehe auch 

- [Gleam Dokumentation zu YAML](https://gleam.run/articles/yaml-processing) 
- [Offizielle YAML-Website](https://yaml.org/) 
- [Eine Einführung in YAML](https://codebeautify.org/yaml-introduction)