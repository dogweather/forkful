---
title:                "Überprüfung, ob ein Verzeichnis existiert"
html_title:           "Elm: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

Was & Warum?:
Das Überprüfen, ob ein Verzeichnis vorhanden ist, ist ein grundlegender Bestandteil der Programmierung. Es ermöglicht Programmierern, zu überprüfen, ob ein bestimmter Ordner bereits existiert, bevor sie weitere Aktionen ausführen. Dadurch können Probleme vermieden werden, die auftreten können, wenn ein Ordner mehrmals erstellt wird.

Wie geht's:
Zum Überprüfen, ob ein Verzeichnis vorhanden ist, können wir die Funktion `Directory.exists` verwenden. Diese Funktion akzeptiert eine Zeichenfolge als Eingabe und gibt ein Bool zurück, das angibt, ob das Verzeichnis vorhanden ist oder nicht. Hier ist ein Beispielcode:

```
Elm Directory.exists "/Users/me/Documents" 
--> True 
```

Hier ist ein Beispiel für den Fall, dass das Verzeichnis nicht vorhanden ist:

```
Elm Directory.exists "/Users/me/Images" 
--> False
```

Tiefere Einblicke:
Das Überprüfen von Verzeichnissen ist ein wichtiger Bestandteil der Dateiverwaltung in der Programmierung. Früher war die Überprüfung von Verzeichnissen oft komplizierter und fehleranfälliger, da Programmierer eigene Methoden entwickeln mussten, um auf das Dateisystem zuzugreifen. Dank Funktionen wie `Directory.exists` ist dieser Prozess jedoch sehr viel einfacher geworden. Es gibt auch alternative Methoden zum Überprüfen von Verzeichnissen, z.B. die Verwendung von Dateisystem-Monad oder das Auslösen von Ausnahmen, wenn das Verzeichnis nicht gefunden wird.

Siehe auch:
- Elm-Dokumentation von `Directory.exists`: https://package.elm-lang.org/packages/elm/file/latest/Directory#exists
- Alternative Methoden zum Überprüfen von Verzeichnissen in Elm: https://discourse.elm-lang.org/t/check-if-directory-exists/4809/3