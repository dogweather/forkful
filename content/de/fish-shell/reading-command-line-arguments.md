---
title:                "Befehlszeilenargumente lesen"
html_title:           "Arduino: Befehlszeilenargumente lesen"
simple_title:         "Befehlszeilenargumente lesen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was & Warum?
Kommandozeilenargumente werden beim Ausführen eines Programms übergeben und können das Verhalten des Programms steuern. Programmierer nutzen diese Funktion, um ihre Programme flexibler und wiederverwendbarer zu machen.

## So geht's
Fish Shell macht es sehr einfach, Kommandozeilenargumente zu lesen. Hier ist ein einfaches Beispiel:

```fish
function greet
  echo "Hallo, $argv[1]"
end
```

Ausführung und Ausgabe:

```fish
> greet Welt
Hallo, Welt
```

In diesem Beispiel enthält `argv[1]` das erste Argument, das an die Funktion übergeben wird.

## Vertiefung
Kommandozeilenargumente gibt es schon lange in der Programmierung. Ursprünglich wurden sie in Shell-Skripts verwendet, aber jetzt sind sie in den meisten modernen Programmiersprachen vorhanden.

Es gibt auch andere Wege, Argumente in Fish Shell zu verarbeiten, z.B. mit der `for-in`-Schleife:

```fish
function greet_all
  for name in $argv
    echo "Hallo, $name"
  end
end
```

Aber egal, welche Methode Sie verwenden, die Essenz bleibt gleich: Sie können das Verhalten Ihres Programms steuern, indem Sie Argumente von der Kommandozeile lesen.

## Weiterführende Informationen
Für weitere Informationen über die Fish Shell und ihre Funktionalität, besuchen Sie die offizielle Dokumentation ([Fish Shell Dokumentation](https://fishshell.com/docs/current/index.html)) und das Fish Shell GitHub Repository ([Fish Shell auf GitHub](https://github.com/fish-shell/fish-shell)). Hier finden Sie mehr Beispiele und detailliertere Erklärungen.