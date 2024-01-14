---
title:                "Fish Shell: Löschen von Zeichen, die mit einem Muster übereinstimmen"
simple_title:         "Löschen von Zeichen, die mit einem Muster übereinstimmen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann in der Programmierung sehr hilfreich sein, um unerwünschte Zeichen in Texten oder Dateien zu entfernen. Mit der Fish Shell können wir dies einfach und effektiv erreichen.

# Wie geht das?

Mit der Fish Shell können wir das Muster mit dem Befehl `expr` überprüfen und dann mit dem `sed` Befehl die Zeichen entsprechend löschen. Hier ist ein Beispiel:

```Fish Shell
set text "Hallo Welt123!"
set pattern [0-9!]
expr $text : "*$pattern"
set result (sed "s/$pattern//g" <<< $text)
echo $result
```

Dieses Beispiel zeigt, wie wir das Muster `[0-9!]` verwenden, um alle Zahlen und das Ausrufezeichen aus dem Text "Hallo Welt123!" zu löschen. Der Befehl `expr` überprüft, ob das Muster im Text vorhanden ist, und der Befehl `sed` löscht dann alle passenden Zeichen.

Das Ergebnis dieses Codes ist "Hallo Welt", da alle Zahlen und das Ausrufezeichen erfolgreich gelöscht wurden.

# Tiefer Einblick

Das Löschen von Zeichen auf diese Weise kann sehr nützlich sein, wenn wir zum Beispiel Dateinamen bereinigen müssen. Mit dem `sed` Befehl können wir die zu löschenden Zeichen und Muster flexibel anpassen, um sie an unsere Bedürfnisse anzupassen.

## `expr` Befehl

Der `expr` Befehl ist in der Fish Shell sehr praktisch, da er uns ermöglicht, einfache Muster in Strings zu überprüfen. Es ist wichtig zu beachten, dass das Muster mit den Wildcard-Zeichen `*` und `?` geschrieben werden muss, um alle passenden Zeichen zu finden.

## `sed` Befehl

Der `sed` Befehl ist sehr flexibel und kann neben dem Löschen von Zeichen auch sehr komplexe Textmanipulationen durchführen. In diesem Beispiel haben wir den Befehl verwendet, um das Muster im Text zu löschen, aber er hat noch viele weitere Funktionen, die es sich lohnt zu erforschen.

# Siehe auch

- [Fish Shell Dokumentation](https://fishshell.com/docs/current/index.html)
- [Fish Shell Tutorial (auf Deutsch)](https://www.linux-magazin.de/ausgaben/2017/05/fish-shell/)