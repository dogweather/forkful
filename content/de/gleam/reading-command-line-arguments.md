---
title:                "Gleam: Lesen von Befehlszeilenargumenten"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie Gleam-Programmierer sind, müssen Sie häufig mit Echtwerten arbeiten, die vom Benutzer über die Befehlszeilenschnittstelle eingegeben werden. In diesem Blogbeitrag werde ich Ihnen zeigen, wie Sie Command-Line-Argumente in Gleam lesen und verarbeiten können, um Ihre Programme interaktiver zu gestalten.

## Anleitung

Das Lesen von Befehlszeilenargumenten in Gleam ist eine einfache Aufgabe. Zuerst müssen Sie das Standardmodul "os" importieren, um auf die Befehlszeilenargumente zugreifen zu können.

```Gleam
import os

```

Dann können Sie die Befehlszeilenargumente mit der Funktion `os.args()` in einer Liste von Strings erhalten. Sie können diese Liste dann durchlaufen und die Argumente mit Ihren Programmausgaben verarbeiten.

```Gleam
fn main() {
  args := os.args()

  for arg in args {
    println("Received argument: ${arg}")
  }
}

```

Angenommen, Sie starten Ihr Programm mit dem Befehl `gleam run my_program.gleam hello world`, dann wird die Ausgabe wie folgt sein:

```
Received argument: hello
Received argument: world
```

Sie können auch auf einzelne Argumente zugreifen, indem Sie auf die entsprechenden Indizes in der Liste `args` verweisen. Zum Beispiel, wenn Sie nur das zweite Argument benötigen, können Sie `args[1]` verwenden.

## Deep Dive

Sie können auch benutzerdefinierte Argumente für Ihr Programm definieren, indem Sie Optionen und Werte im Befehlszeilenformat übergeben. Beispielsweise können Sie die Option `-n` verwenden, um den Namen des Benutzers einzugeben, und die Option `-a` für das Alter. Um diese Argumente in Gleam zu lesen, können Sie das externe Paket "clap" verwenden. Dieses Paket ermöglicht es Ihnen, komplexe Befehlszeilenargumente mit verschiedenen Typen zu definieren.

Weitere Informationen zu "clap" und wie Sie es in Ihre Gleam-Projekte einbinden können, finden Sie in der offiziellen Dokumentation.

## Siehe auch

- Offizielle Dokumentation für Gleam: https://gleam.run/getting-started/
- Externes Paket "clap" für Gleam: https://github.com/gleam-lang/clap
- Beispielprojekt für Gleam mit Befehlszeilenargumenten: https://github.com/gleam-lang/gleam-cli-example