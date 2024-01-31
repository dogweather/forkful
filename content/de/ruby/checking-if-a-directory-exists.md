---
title:                "Überprüfung, ob ein Verzeichnis existiert"
date:                  2024-01-20T14:58:27.377057-07:00
html_title:           "Fish Shell: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"

category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Was & Warum?)
Das Überprüfen, ob ein Verzeichnis existiert, bedeutet zu kontrollieren, ob ein spezifischer Ordner im Dateisystem vorhanden ist. Programmierer tun dies, um Fehler zu vermeiden, die auftreten, wenn man versucht, in ein nicht vorhandenes Verzeichnis zu lesen oder zu schreiben.

## How to: (Wie macht man das:)
Ruby macht es uns leicht, die Existenz eines Verzeichnisses zu überprüfen. Hier sind zwei schlanke Methoden, die du verwenden kannst:

```Ruby
require 'fileutils'

# Methode 1: File.directory?
puts File.directory?('dein/verzeichnis') ? 'Existiert!' : 'Existiert nicht!'

# Methode 2: Dir.exist?
puts Dir.exist?('dein/verzeichnis') ? 'Existiert!' : 'Existiert nicht!'
```

Beispieloutput für beide Methoden, wenn das Verzeichnis existiert:
```
Existiert!
```

Wenn das Verzeichnis nicht existiert:
```
Existiert nicht!
```

## Deep Dive (Tiefer eintauchen)
Historisch gesehen war die Überprüfung auf ein Verzeichnis schon immer Teil der Standardbibliotheken von Ruby. Frühere Versionen von Ruby verwendeten `File.exists?`, welche jedoch veraltet und durch `File.exist?` ersetzt wurde. `File.directory?` ist spezifischer, da es sicherstellt, dass der Pfad tatsächlich ein Verzeichnis ist und keine Datei.

Alternativ könntest du auch niedrigerstufige Bibliotheken wie `Pathname` verwenden oder gar system-spezifische Befehle ausführen, obwohl dies weniger portabel ist. Die Methoden `File.directory?` und `Dir.exist?` sind jedoch ausreichend und eingebettet in Ruby's intuitive Art der Problembehandlung, also brauchst du normalerweise nicht weiterzusuchen.

Im Kontext der Implementierung, wenn du etwa mit Webanwendungen arbeitest, prüfst du vielleicht Verzeichnisse auf hochgeladene Dateien oder erstellst dynamisch neue Verzeichnisse für Benutzerdaten. Eine solche Überprüfung ist essentiell, um sicherzustellen, dass deine Anwendung robust und widerstandsfähig gegen fehlerbedingte Unterbrechungen ist.

## See Also (Siehe auch)
- Ruby-Dokumentation zu `Dir.exist?`: https://ruby-doc.org/core-2.7.0/Dir.html#method-c-exist-3F
- Ruby-Dokumentation zu `File.directory?`: https://ruby-doc.org/core-2.7.0/File.html#method-c-directory-3F
- FileUtils Modul-Dokumentation: https://ruby-doc.org/stdlib-2.7.0/libdoc/fileutils/rdoc/FileUtils.html

Immer daran denken, die Version der Dokumentation anzupassen, falls du nicht die aktuellste Ruby-Version verwendest.
