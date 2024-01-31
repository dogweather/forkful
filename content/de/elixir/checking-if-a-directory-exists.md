---
title:                "Überprüfung, ob ein Verzeichnis existiert"
date:                  2024-01-20T14:56:14.336265-07:00
html_title:           "Fish Shell: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?
Prüfen, ob ein Verzeichnis existiert, heißt zu sehen, ob ein bestimmter Ordner auf dem Dateisystem vorhanden ist. Programmierer machen das, um Fehler beim Zugriff auf nicht existierende Verzeichnisse zu vermeiden oder um dynamisch mit Dateisystemstrukturen zu arbeiten.

## Wie geht das:
```elixir
# Verwenden von File.dir?/1, um zu prüfen, ob ein Verzeichnis existiert
def check_directory_exists(directory) do
  File.dir?(directory)
end

# Beispiel Nutzung
is_directory_present = check_directory_exists("/mein/pfad/zu/pruefen")
IO.puts(is_directory_present) # Gibt 'true' oder 'false' aus
```
Sample Output:
```
true
```
Wenn es "`true`" ausgibt, existiert das Verzeichnis. "`false`" bedeutet das Gegenteil.

## Tiefgang
Historisch gesehen beruht die Möglichkeit, Dateisystem-Checks durchzuführen, auf Betriebssystemfunktionen. In Elixir stellt das `File` Modul viele Funktionen bereit, um das Dateisystem zu handhaben. `File.dir?/1` ist speziell dafür gedacht, rasch zu überprüfen, ob ein Pfad ein Verzeichnis ist. Alternativen in anderen Sprachen beinhalten Funktionen wie `os.path.isdir()` in Python oder `fs.existsSync()` in Node.js. Implementierungsdetails hängen von der zugrundeliegenden Betriebssystem-API und der Elixir-Version ab.

## Siehe auch
- Elixir's offizielle Dokumentation zu File.dir?/1: [https://hexdocs.pm/elixir/File.html#dir?/1](https://hexdocs.pm/elixir/File.html#dir?/1)
- Erlang's :file Modul, auf dem Elixirs File-Modul basiert: [https://erlang.org/doc/man/file.html](https://erlang.org/doc/man/file.html)
- Eine Anleitung zur Fehlerbehandlung beim Zugriff auf das Dateisystem in Elixir: [https://elixir-lang.org/getting-started/io-and-the-file-system.html](https://elixir-lang.org/getting-started/io-and-the-file-system.html)
