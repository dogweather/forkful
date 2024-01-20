---
title:                "Die Länge eines Strings ermitteln"
html_title:           "Java: Die Länge eines Strings ermitteln"
simple_title:         "Die Länge eines Strings ermitteln"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was und Warum?

Die Ermittlung der String-Länge ist das Exzählen von Charakteren in einem bestimmten Textstück. Programmierer tun dies meist, um Speicherplatz effizient zu verwalten und Fehlverhalten von Programmen zu vermeiden. 

## So geht's:

Unter Fish Shell benutzt man `string length -q` Befehl um die Länge eines Strings zu ermitteln. Hier ein Beispiel:

```fish
echo (string length -q "Hallo Welt")
```

Die Ausgabe wäre:

```
11
```
## Vertiefung

Historisch gesehen, haben frühere Versionen der Shell-Programmierung den `wc -m` Befehl verwendet, aber bei Fish Shell haben wir jetzt `string length -q`. Sie müssen sich keine Sorgen machen, es hängt nicht von externen Befehlen ab und es ist in 100% reinem Fish geschrieben.

Es gibt mehrere Weisen, um die Länge eines Strings zu ermitteln, `string split` und `count` sind Beispiele. Aber `string length -q` ist simpler und schneller, deswegen wird sie gewöhnlich bevorzugt.

Die Implementierung von `string length -q` ist recht geradlinig. Es iteriert einfach den gegebenen String und zählt die Anzahl der Charaktere.

## Siehe auch

Für weitere Einzelheiten, schauen Sie sich die Dokumentation an:

1. [Fish `string` Dokumentation](https://fishshell.com/docs/current/cmds/string.html)
2. [Fish Shell Handbuch](https://fishshell.com/docs/current/index.html)