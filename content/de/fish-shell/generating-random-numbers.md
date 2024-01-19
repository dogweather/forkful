---
title:                "Zufallszahlen generieren"
html_title:           "Arduino: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?

Zufällige Zahlen zu erzeugen bedeutet, eine Reihe von Werten zu erstellen, die nicht vorhersehbar sind. Dies ist für Programmierer wichtig, um beispielsweise einzigartige Identifikationscodes zu erzeugen oder Testdaten zu simulieren.

## So geht's:

Mit Fish Shell können Sie mithilfe des Befehls 'random' zufällige Zahlen erstellen. Hier sind ein paar Beispiele dazu:

```Fish Shell
# Erzeugt eine Zufallszahl zwischen 1 und 100
random 1 100
```

Ausgabe:

```Fish Shell
57
```

oder

```Fish Shell
# Erzeugt eine Liste von 5 Zufallszahlen zwischen 1 und 50
for i in (seq 5)
    random 1 50
end
```

Ausgabe:

```Fish Shell
23
2
45
19
36
```

## Hintergrundwissen

Zufallszahlen haben eine lange Geschichte in der Programmierung. Ihre Verwendung zur Erzeugung von einzigartiger Identifikation und Testdaten ist weit verbreitet. Es gibt unterschiedliche Ansätze, um Zufallszahlen zu generieren. Fish Shell bietet einen Ansatz, der auf dem /dev/random des Betriebssystems basiert. 

Als Alternative können Sie auch das Bibliothekmodul 'random' in Python verwenden oder die Funktion 'rand' in C. Die Implementationsdetails können variieren, das Konzept bleibt jedoch das Gleiche. 

## Weiterführende Links

- Fish Shell Dokumentation: https://fishshell.com/docs/current/commands.html#random 
- Python Random Modul: https://docs.python.org/3/library/random.html 
- C rand Funktion: https://en.cppreference.com/w/c/numeric/random/rand