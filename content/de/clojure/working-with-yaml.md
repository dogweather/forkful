---
title:                "Arbeiten mit YAML"
html_title:           "Clojure: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

# Was & Warum?
Arbeiten mit YAML ist eine Möglichkeit, Daten in einer strukturierten und lesbaren Weise zu speichern. Programmierer nutzen es, um Konfigurationsdateien, benutzerdefinierte Datenformate und sogar Übersetzungen im Code zu verwalten.

# Wie geht's?
In **Clojure** gibt es das Paket `yaml-clojure` für das Arbeiten mit YAML. Zuerst musst du es importieren: 
```Clojure
(require '[yaml-clojure.core :as yaml])
```
Um Daten von einer YAML-Datei zu lesen, verwende die Funktion `load` und gebe den Dateipfad als Argument an:
```Clojure
(yaml/load "~/pfad/zur/datei.yml")
```
Um eine Karte in eine YAML-Datei zu schreiben, verwende `dump` und gebe die Karte und den Dateipfad als Argumente an:
```Clojure
(yaml/dump {:name "Max" :age 25} "~/pfad/zur/datei.yml")
```
Die Ausgabe wird in der Datei als YAML-formatierter Text vorhanden sein.

# Tiefer tauchen
YAML wurde ursprünglich entwickelt, um eine einfachere und benutzerfreundlichere Alternative zu XML zu sein. Es ist ein offenes Format und es gibt mehrere Implementierungen für verschiedene Programmiersprachen. Alternativen zu YAML sind JSON und TOML.

Die `yaml-clojure`-Bibliothek verwendet das Java-Bibliothek SnakeYAML, um mit YAML-Daten zu arbeiten. Eine gut formatierte und detaillierte Dokumentation dazu findest du im [Github-Repository](https://github.com/yaml/yaml-clojure).

# Siehe auch
Weitere Informationen zum Arbeiten mit YAML in Clojure findest du in der [offiziellen Dokumentation](https://yaml-clojure.readthedocs.io/en/latest/). Für eine allgemeine Einführung in YAML kannst du die [offizielle Website](https://yaml.org/) besuchen.