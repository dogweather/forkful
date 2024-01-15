---
title:                "Überprüfen, ob ein Verzeichnis existiert"
html_title:           "Fish Shell: Überprüfen, ob ein Verzeichnis existiert"
simple_title:         "Überprüfen, ob ein Verzeichnis existiert"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum man prüfen möchte, ob ein Verzeichnis existiert. Möglicherweise müssen Sie sicherstellen, dass ein bestimmtes Verzeichnis vorhanden ist, bevor Sie Dateien in dieses Verzeichnis schreiben oder aus ihm lesen. Oder Sie möchten einen Befehl nur dann ausführen, wenn ein bestimmtes Verzeichnis vorhanden ist. In jedem Fall ist es wichtig zu wissen, wie man in der Fish Shell überprüft, ob ein Verzeichnis existiert.

## Anleitung

Um zu überprüfen, ob ein Verzeichnis existiert, können Sie den Befehl `test` verwenden. Mit diesem Befehl können Sie verschiedene Bedingungen überprüfen, einschließlich der Existenz eines Verzeichnisses. Hier ist ein Beispiel, wie Sie überprüfen können, ob das Verzeichnis "Dokumente" in Ihrem aktuellen Verzeichnis vorhanden ist:

```Fish Shell
if test -d Documents
    echo "Das Verzeichnis 'Dokumente' existiert."
else
    echo "Das Verzeichnis 'Dokumente' existiert nicht."
end
```

Wenn das Verzeichnis vorhanden ist, gibt der Befehl "test" den Wert True zurück und die Ausgabe "Das Verzeichnis 'Dokumente' existiert." wird angezeigt. Andernfalls wird der Wert False zurückgegeben und die Ausgabe "Das Verzeichnis 'Dokumente' existiert nicht." wird angezeigt.

## Tief ein tauchen

Das Prüfen der Existenz eines Verzeichnisses kann auch in Kombination mit anderen Bedingungen durchgeführt werden. Zum Beispiel können Sie mit dem Befehl `and` oder `or` mehrere Bedingungen zusammenführen. Hier ist ein Beispiel, wie Sie überprüfen können, ob sowohl das Verzeichnis "Dokumente" als auch das Verzeichnis "Bilder" vorhanden sind:

```Fish Shell
if test -d Documents and test -d Pictures
    echo "Sowohl das Verzeichnis 'Dokumente' als auch das Verzeichnis 'Bilder' existieren."
else
    echo "Entweder das Verzeichnis 'Dokumente' oder das Verzeichnis 'Bilder' existiert nicht."
end
```

Sie können auch `not` verwenden, um eine Bedingung zu negieren. Hier ist ein Beispiel, wie Sie überprüfen können, ob das Verzeichnis "Downloads" nicht vorhanden ist:

```Fish Shell
if not test -d Downloads
    echo "Das Verzeichnis 'Downloads' existiert nicht."
end
```

Es ist auch möglich, die Ausgabe von `test` auf eine Variable zu speichern und später zu verwenden. Hier ist ein Beispiel, wie Sie eine Bedingung überprüfen und basierend auf dem Ergebnis eine Aktion ausführen können:

```Fish Shell
set directory_exists (test -d Music; and; echo $status)
if $directory_exists
    echo "Das Verzeichnis 'Musik' existiert."
else
    echo "Das Verzeichnis 'Musik' existiert nicht."
end
```

In diesem Fall wird die Ausgabe von `test` in der Variable `directory_exists` gespeichert. Wenn das Verzeichnis vorhanden ist, wird der Variable der Wert 0 zugewiesen, andernfalls wird ihr der Wert 1 zugewiesen. Sie können dann diesen Wert verwenden, um eine Aktion auszuführen.

## Siehe Auch

Weitere Informationen zu Bedingungen und dem Befehl `test` finden Sie in der [offiziellen Fish Shell-Dokumentation](https://fishshell.com/docs/current/cmds/test.html). Sie können auch mehr über die [Verwendung von Variablen](https://fishshell.com/docs/current/index.html#variables) und [Bedingungen](https://fishshell.com/docs/current/index.html#conditions-and-loops) in der Fish Shell erfahren.