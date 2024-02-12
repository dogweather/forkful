---
title:                "Überprüfung, ob ein Verzeichnis existiert"
aliases:
- /de/elixir/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:05.816405-07:00
model:                 gpt-4-0125-preview
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Das Überprüfen, ob ein Verzeichnis in Elixir existiert, bezieht sich darauf, die Anwesenheit eines Verzeichnisses an einem bestimmten Pfad im Dateisystem zu verifizieren. Programmierer tun dies, um sicherzustellen, dass sie sicher aus dem Verzeichnis lesen, in es schreiben oder Operationen darauf ausführen können, ohne auf Fehler wegen seiner Abwesenheit zu stoßen.

## Wie:
Die Standardbibliothek von Elixir bietet eine unkomplizierte Möglichkeit, die Existenz eines Verzeichnisses über das `File` Modul zu überprüfen. Hier ist, wie Sie es nutzen können:

```elixir
if File.dir?("path/to/directory") do
  IO.puts "Verzeichnis existiert!"
else
  IO.puts "Verzeichnis existiert nicht."
end
```

Beispieloutput, unter der Annahme, dass das Verzeichnis nicht existiert:
```
Verzeichnis existiert nicht.
```

Für fortgeschrittenere Interaktionen mit dem Dateisystem, einschließlich der Überprüfung der Existenz von Verzeichnissen, könnten Sie darüber nachdenken, Drittanbieterbibliotheken wie `FileSystem` zu verwenden. Obwohl die Standardfähigkeiten von Elixir für viele Fälle ausreichen, kann `FileSystem` eine nuanciertere Kontrolle und Rückmeldung für komplexe Anwendungen bieten. Jedoch ist für das grundlegende Bedürfnis, zu überprüfen, ob ein Verzeichnis existiert, das Festhalten am nativen `File` Modul in der Regel empfohlen, da es sofort verfügbar ist und keine externen Abhängigkeiten erfordert.
