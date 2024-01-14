---
title:                "Elixir: Prüfen, ob ein Ordner existiert"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie jemals in Ihrem Elixir-Projekt Dateien oder Ordner erstellen oder lesen müssen, werden Sie wahrscheinlich an einem Punkt auf die Notwendigkeit stoßen, zu überprüfen, ob ein bestimmter Ordner existiert. Dieser Prozess kann Ihnen helfen, Fehler in Ihrem Code zu vermeiden, indem Sie sicherstellen, dass die erforderlichen Ordner vorhanden sind, bevor Sie versuchen, mit ihnen zu arbeiten.

## Wie man es macht

Das Überprüfen der Existenz eines Ordners in Elixir ist relativ einfach. Alles was Sie tun müssen, ist die Funktion `File.dir?/1` zu verwenden und den Pfad des zu überprüfenden Ordners als Argument einzugeben.

```Elixir
File.dir?("ordner/Pfad")
# => true
```

Wenn der Ordner existiert, gibt die Funktion `true` zurück, andernfalls gibt sie `false` zurück.

## Tiefer Einblick

Wenn Sie sich fragen, wie genau die `File.dir?/1`-Funktion funktioniert, wirft ein Blick in das Elixir-Quellcode-Repository einige interessante Erkenntnisse auf:

- Die Funktion verwendet die `File.list_dir/1`-Funktion, um eine Liste der Ordner im angegebenen Pfad abzurufen.
- Anschließend wird eine Liste der Dateien im angegebenen Pfad mit `File.stat!/1` erstellt.
- Schließlich wird überprüft, ob der angegebene Ordner in der Liste der Ordner enthalten ist und gibt entsprechend `true` oder `false` zurück.

Es ist auch erwähnenswert, dass bei der Verwendung der `File.dir?/1`-Funktion Pfade entweder absolut oder relativ sein können. Dies ermöglicht eine flexible Verwendung in verschiedenen Szenarien.

## Siehe auch

- [Offizielle Elixir-Dokumentation zu Dateien](https://hexdocs.pm/elixir/File.html)
- [Elixir-Quellcode-Repository](https://github.com/elixir-lang/elixir)