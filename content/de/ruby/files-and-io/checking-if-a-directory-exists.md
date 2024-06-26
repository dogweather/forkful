---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:13.542914-07:00
description: "Wie: Die Standardbibliothek von Ruby bietet einfache Methoden, um die\
  \ Existenz eines Verzeichnisses zu \xFCberpr\xFCfen. So machen Sie das mit reinem\
  \ Ruby, ohne\u2026"
lastmod: '2024-03-13T22:44:54.414784-06:00'
model: gpt-4-0125-preview
summary: "Die Standardbibliothek von Ruby bietet einfache Methoden, um die Existenz\
  \ eines Verzeichnisses zu \xFCberpr\xFCfen."
title: "\xDCberpr\xFCfung, ob ein Verzeichnis existiert"
weight: 20
---

## Wie:
Die Standardbibliothek von Ruby bietet einfache Methoden, um die Existenz eines Verzeichnisses zu überprüfen. So machen Sie das mit reinem Ruby, ohne dass Sie Drittbibliotheken benötigen:

```ruby
require 'fileutils'

# Überprüfen, ob ein Verzeichnis existiert
if Dir.exist?('/pfad/zum/verzeichnis')
  puts 'Verzeichnis existiert.'
else
  puts 'Verzeichnis existiert nicht.'
end
```
Beispiel-Ausgabe:
```
Verzeichnis existiert.
```
Oder:
```
Verzeichnis existiert nicht.
```

Zusätzlich zur Verwendung von `Dir.exist?` können Sie auch die Methode `File.directory?` nutzen, die `true` zurückgibt, wenn der angegebene Pfad ein Verzeichnis ist:

```ruby
if File.directory?('/pfad/zum/verzeichnis')
  puts 'Verzeichnis existiert.'
else
  puts 'Verzeichnis existiert nicht.'
end
```
Sowohl `Dir.exist?` als auch `File.directory?` sind Teil der Standardbibliothek von Ruby und erfordern keine externen Gems, was sie zu bequemen und effizienten Optionen für die Überprüfung von Verzeichnissen macht.
