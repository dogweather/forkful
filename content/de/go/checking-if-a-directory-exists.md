---
title:                "Überprüfen, ob ein Verzeichnis vorhanden ist"
html_title:           "Go: Überprüfen, ob ein Verzeichnis vorhanden ist"
simple_title:         "Überprüfen, ob ein Verzeichnis vorhanden ist"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum 
Let's face it, checking if a directory exists is not the most exciting task in programming. But it is a crucial one, especially when working with files and folders. By knowing how to perform this simple task in Go, you can save yourself from potential errors and ensure the smooth running of your code.

## Wie geht es 
Um zu überprüfen, ob ein Verzeichnis in Go existiert, gibt es einige verschiedene Möglichkeiten. In den folgenden Codebeispielen verwenden wir die Funktion `os.Stat()` und die `os.IsNotExist()` Funktion, um zu überprüfen, ob das angegebene Verzeichnis existiert oder nicht.

Um zu beginnen, importieren wir das `os` Paket in unserem Code:

```Go
import "os"
```

Als nächstes verwenden wir die `os.Stat()` Funktion, um Informationen über ein gegebenes Verzeichnis abzurufen. Diese Funktion gibt eine `FileInfo` Struktur zurück, die Informationen wie Dateigröße, Erstellungsdatum und Änderungsdatum enthält.

```Go
info, err := os.Stat("pfad/zum/verzeichnis")
```

Wir überprüfen nun, ob es einen Fehler beim Aufrufen der `os.Stat()` Funktion gab, indem wir die `err` Variable prüfen. Falls der Fehler `nil` ist, bedeutet dies, dass das Verzeichnis existiert und wir können weiter mit unserem Code fortfahren. Ist der Fehler jedoch nicht `nil`, bedeutet dies, dass das Verzeichnis nicht existiert und wir können entsprechend reagieren.

```Go
if err != nil {
    // Verzeichnis existiert nicht
} else {
    // Verzeichnis existiert 
}
```

Eine weitere Möglichkeit ist die Verwendung der `os.IsNotExist()` Funktion. Diese Funktion nimmt den Rückgabewert von `os.Stat()` entgegen und überprüft, ob der Fehler auf eine nicht vorhandene Datei oder ein Verzeichnis hinweist.

```Go
// Überprüfen auf Fehler
if os.IsNotExist(err) {
    // Verzeichnis existiert nicht
} else {
    // Fehler ist kein Hinweis auf nicht existierendes Verzeichnis
    // Verzeichnis existiert vielleicht doch
}
```

## Tiefer Einblick 
Bei der Verwendung von `os.Stat()` gibt es einige wichtige Dinge zu beachten. Zum Beispiel werden keine Fehler zurückgegeben, wenn der angegebene Pfad ein symbolischer Link ist, der auf ein nicht existierendes Verzeichnis verweist. In solchen Fällen wird die tatsächliche Existenz des Verzeichnisses durch den `os.Stat()` Aufruf nicht überprüft.

Eine weitere wichtige Sache ist, dass die `os.Stat()` Funktion auch eine Fehlermeldung zurückgibt, wenn der Zugriff auf das Verzeichnis nicht möglich ist, zum Beispiel aufgrund von fehlenden Berechtigungen. Daher ist es ratsam, vor dem Aufrufen der `os.Stat()` Funktion sicherzustellen, dass der Zugriff auf das Verzeichnis möglich ist.

## Siehe auch 
- [Paket os in der Go-Dokumentation](https://golang.org/pkg/os/)
- [Tutorial zur Datei- und Verzeichnisverwaltung in Go](https://www.digitalocean.com/community/tutorials/how-to-manage-files-and-directories-in-go-de)
- [Der Unterschied zwischen os.IsNotExist und os.Stat bei der Überprüfung auf Dateiexistenz in Go](https://stackoverflow.com/questions/12518876/how-to-check-if-a-file-exists-in-go-programming/58104833#58104833)