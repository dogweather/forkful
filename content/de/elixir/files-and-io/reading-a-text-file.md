---
aliases:
- /de/elixir/reading-a-text-file/
date: 2024-01-20 17:54:03.339646-07:00
description: "Das Einlesen von Textdateien erm\xF6glicht es, Inhalt von der Festplatte\
  \ in eine Anwendung zu laden. Programmierer ben\xF6tigen diese Funktionalit\xE4\
  t, um Datei-\u2026"
lastmod: 2024-02-18 23:09:04.570274
model: gpt-4-1106-preview
summary: "Das Einlesen von Textdateien erm\xF6glicht es, Inhalt von der Festplatte\
  \ in eine Anwendung zu laden. Programmierer ben\xF6tigen diese Funktionalit\xE4\
  t, um Datei-\u2026"
title: Textdatei einlesen
---

{{< edit_this_page >}}

## Was & Warum?
Das Einlesen von Textdateien ermöglicht es, Inhalt von der Festplatte in eine Anwendung zu laden. Programmierer benötigen diese Funktionalität, um Datei-basierte Daten zu verarbeiten, zu analysieren oder um Konfigurationen zu laden.

## Vorgehensweise:
Um eine Textdatei in Elixir zu lesen, nutzen Sie die `File`-Modul. Hier ist ein einfaches Beispiel:

```elixir
File.read("meine_datei.txt")
|> case do
  {:ok, inhalt} -> IO.puts("Inhalt der Datei: #{inhalt}")
  {:error, reason} -> IO.puts("Fehler beim Lesen der Datei: #{reason}")
end
```

Ausgabe, falls erfolgreich:
```
Inhalt der Datei: Hallo, Welt!
```

Ausgabe, wenn ein Fehler auftritt:
```
Fehler beim Lesen der Datei: enoent
```

## Tiefer Eintauchen:
Die `File.read/1`-Funktion ist nur die Spitze des Eisbergs. Historisch gesehen kommen Dateisystem-Operationen aus Systemaufrufen des Betriebssystems. Elixir verpackt diese in benutzerfreundliche Funktionen. Es gibt auch `File.stream!/3`, was für große Dateien praktisch ist, da es sie in einem Stream und nicht auf einmal einliest. Andernfalls könnten zu große Dateien den Arbeitsspeicher überfüllen. `File.stream!/3` liest Datei-Inhalte Zeile für Zeile, was den Speicherverbrauch minimiert. Ein Beispiel:

```elixir
File.stream!("große_datei.txt")
|> Enum.each(fn zeile -> IO.puts(zeile) end)
```

Außerdem können Sie mit `IO` Module direkt lesen, wenn Sie einen feinkörnigeren Zugriff oder Echtzeit-Operationen benötigen. Beispielsweise `IO.binread/2` für Binärdateien.

## Siehe Auch:
- [Elixirs offizielle File-Modul-Dokumentation](https://hexdocs.pm/elixir/File.html)
- [IO-Modul-Dokumentation](https://hexdocs.pm/elixir/IO.html)
- [Erstellen von Streams in Elixir](https://hexdocs.pm/elixir/Stream.html)
