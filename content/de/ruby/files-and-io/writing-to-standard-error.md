---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:45.851299-07:00
description: "Wie geht das: Die Standardbibliothek von Ruby bietet eine unkomplizierte\
  \ M\xF6glichkeit, auf stderr zu schreiben, und zwar mit `$stderr` oder `STDERR`.\
  \ F\xFCr\u2026"
lastmod: '2024-03-13T22:44:54.416833-06:00'
model: gpt-4-0125-preview
summary: "Die Standardbibliothek von Ruby bietet eine unkomplizierte M\xF6glichkeit,\
  \ auf stderr zu schreiben, und zwar mit `$stderr` oder `STDERR`."
title: Schreiben auf Standardfehler
weight: 25
---

## Wie geht das:
Die Standardbibliothek von Ruby bietet eine unkomplizierte Möglichkeit, auf stderr zu schreiben, und zwar mit `$stderr` oder `STDERR`. Für diesen grundlegenden Vorgang sind keine Drittanbieterbibliotheken erforderlich.

### Eine einfache Nachricht auf stderr schreiben:
```ruby
$stderr.puts "Fehler: Datei nicht gefunden."
# Oder gleichwertig
STDERR.puts "Fehler: Datei nicht gefunden."
```
Beispielausgabe (auf stderr):
```
Fehler: Datei nicht gefunden.
```

### Umleitung von stderr in eine Datei:
```ruby
File.open('error.log', 'w') do |file|
  STDERR.reopen(file)
  STDERR.puts "Fehler beim Öffnen der Konfiguration."
end
```
Dieser Codeausschnitt leitet stderr in eine Datei namens `error.log` um, und alle nachfolgend geschriebenen Fehler werden dort ausgegeben, bis das Programm die Umleitung von stderr zurücksetzt oder beendet.

### Verwendung von stderr bei der Ausnahmebehandlung:
```ruby
begin
  # Simulation eines Vorgangs, der fehlschlagen könnte, z. B. das Öffnen einer Datei
  File.open('nonexistent_file.txt')
rescue Exception => e
  STDERR.puts "Ausnahme aufgetreten: #{e.message}"
end
```
Beispielausgabe (auf stderr):
```
Ausnahme aufgetreten: No such file or directory @ rb_sysopen - nonexistent_file.txt
```

Obwohl die eingebauten Methoden von Ruby für das Schreiben auf stderr für viele Anwendungen ausreichen, könnte man für komplexere Protokollierungsbedürfnisse die Standardbibliothek `logger` oder externe Gems wie `Log4r` in Betracht ziehen. Diese bieten konfigurierbare Protokollierungsmechanismen, einschließlich Schweregradstufen, Formatierung und der Fähigkeit, auf verschiedene Ausgaben zu schreiben, einschließlich Dateien, E-Mail und mehr.
