---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:52.230256-07:00
description: "Wie geht das: Ruby macht Dateioperationen unkompliziert. Um in eine\
  \ Datei zu schreiben, k\xF6nnen Sie Rubys eingebaute `File`-Klasse verwenden. Das\
  \ folgende\u2026"
lastmod: '2024-03-13T22:44:54.418797-06:00'
model: gpt-4-0125-preview
summary: Ruby macht Dateioperationen unkompliziert.
title: Eine Textdatei schreiben
weight: 24
---

## Wie geht das:
Ruby macht Dateioperationen unkompliziert. Um in eine Datei zu schreiben, können Sie Rubys eingebaute `File`-Klasse verwenden. Das folgende Beispiel demonstriert, wie eine Datei zum Schreiben (`"w"`-Modus) und zum Anhängen (`"a"`-Modus) geöffnet wird, dann wie ein String hineingeschrieben wird und sicherstellt, dass die Datei danach geschlossen wird:

```ruby
# Neuen Inhalt in eine Datei schreiben, bestehenden Inhalt überschreiben
File.open("example.txt", "w") do |file|
  file.puts "Hallo, Ruby!"
end

# Inhalt an das Ende einer Datei anhängen
File.open("example.txt", "a") do |file|
  file.puts "Eine weitere Zeile hinzufügen."
end
```
Nachdem beide Schnipsel ausgeführt wurden, wird der Inhalt von `example.txt` sein:
```
Hallo, Ruby!
Eine weitere Zeile hinzufügen.
```

### Nutzung einer Drittanbieter-Bibliothek: FileUtils
Für komplexere Dateioperationen kann die Ruby-Standardbibliothek `FileUtils` nützlich sein, obwohl für das grundlegende Schreiben in Dateien die Standard-`File`-Methoden ausreichen. Wenn Sie jedoch kopieren, verschieben, entfernen oder andere Dateisystemoperationen in Verbindung mit dem Schreiben in Dateien ausführen möchten, ist `FileUtils` eine Überlegung wert.

Ein Beispiel für die Verwendung von `FileUtils` zum Erstellen eines Verzeichnisses und anschließendem Schreiben in eine Datei innerhalb dieses Verzeichnisses:
```ruby
require 'fileutils'

FileUtils.mkdir_p 'logs'
File.open("logs/heute.log", "w") do |file|
  file.puts "Logeintrag: #{Time.now}"
end
```

Dies demonstriert das Erstellen eines neuen Verzeichnisses `logs`, falls es noch nicht existiert, und das Schreiben in eine neue Datei `heute.log` darin. Es zeigt sowohl die Manipulation von Verzeichnissen als auch von Dateien, ohne direkt mit FileUtils zu schreiben, nutzt aber dessen Verzeichnisbehandlungsfähigkeit.
