---
title:                "Ruby: Überprüfen, ob ein Verzeichnis existiert"
simple_title:         "Überprüfen, ob ein Verzeichnis existiert"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum
Eine der grundlegenden Fähigkeiten in der Ruby-Programmierung ist die Überprüfung, ob ein Verzeichnis existiert. Dies ist besonders wichtig, wenn man mit Dateien arbeitet und sicherstellen möchte, dass der Code auf die richtigen Dateien zugreift. In diesem Blogpost erfährst du, wie du mit Ruby prüfen kannst, ob ein Verzeichnis existiert.

## Wie geht man vor
Das Überprüfen eines Verzeichnisses auf Existenz ist ein einfacher Vorgang in der Ruby-Syntax. Hier ist ein Beispiel, wie man das prüfen kann:

```Ruby
exists = Dir.exist?('Pfad/zu/Verzeichnis')
puts exists
```

In diesem Beispiel verwenden wir die eingebaute Methode `Dir.exist?`, um zu prüfen, ob das Verzeichnis "Pfad/zu/Verzeichnis" existiert. Diese Methode gibt einen Boolean (wahr/falsch) Wert zurück, der in der Variable `exists` gespeichert ist. Und schließlich geben wir den Wert mit `puts` aus.

Wenn das Verzeichnis existiert, wird `true` ausgegeben. Falls es nicht existiert, wird `false` ausgegeben.

## Tieferer Einblick
Die eingebaute Methode `Dir.exist?` verwendet das Betriebssystem, um zu überprüfen, ob das Verzeichnis tatsächlich existiert. Sie gibt immer den aktuellen Status zurück und berücksichtigt dabei mögliche Änderungen an dem Verzeichnis.

Außerdem kann man mit dieser Methode auch überprüfen, ob ein Verzeichnis schreibgeschützt ist. Wenn das Verzeichnis schreibgeschützt ist, wird `false` zurückgegeben.

## Siehe auch
- Offizielle Ruby-Dokumentation zu `Dir.exist?`: https://ruby-doc.org/core-2.7.1/Dir.html#method-c-exist-3F
- Beispiel für das Arbeiten mit Dateien und Verzeichnissen in Ruby: https://www.codecademy.com/articles/ruby-file-system
- Weitere Informationen über die Ruby-Syntax: https://ruby-lang.org/de/documentation/