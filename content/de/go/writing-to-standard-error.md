---
title:                "Go: Auf Standardfehler schreiben"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

##Warum

Das Schreiben von Fehlern auf die Standardausgabe, oder auch standard error genannt, ist eine wichtige Technik beim Programmieren in Go. Indem man Fehler auf die Standardausgabe schreibt, können Entwickler Informationen über mögliche Probleme im Code erhalten und diese schnell beheben. In diesem Beitrag werden wir diskutieren, warum das Schreiben auf die Standardausgabe unerlässlich ist und wie man es in Go implementiert.

##So geht's

Um Fehler auf die Standardausgabe zu schreiben, verwenden wir in Go die Funktion `fmt.Fprintf()`. Diese Funktion akzeptiert als ersten Parameter einen `io.Writer` und als zweiten Parameter einen String. Der `io.Writer` ist der Stream, auf den die Nachricht geschrieben werden soll, in unserem Fall die Standardausgabe. Der String enthält die Nachricht oder den Fehler, den wir auf die Standardausgabe schreiben möchten. Es ist wichtig zu beachten, dass der String ein Formatierungsstring sein kann, ähnlich wie bei der `fmt.Printf()` Funktion.

```Go
fmt.Fprintf(os.Stderr, "Fehler aufgetreten: %s", err)
```

In diesem Beispiel verwenden wir `os.Stderr` als Stream und geben die Fehlermeldung aus dem Variablenwert `err` aus. Am Ende des Strings geben wir `%s` an, um anzuzeigen, wo der Wert von `err` eingefügt werden soll. Das Interface `io.Writer` ist implementiert von `os.Stderr`, daher können wir es in `fmt.Fprintf()` verwenden.

Um benutzerdefinierte Fehlermeldungen auf die Standardausgabe zu schreiben, können wir auch den `errors.New()` Konstruktor verwenden:

```Go
err := errors.New("Dies ist eine benutzerdefinierte Fehlermeldung")
fmt.Fprintf(os.Stderr, "Fehler aufgetreten: %s", err)
```

In diesem Beispiel erstellen wir eine neue Fehlermeldung und geben sie mithilfe von `fmt.Fprintf()` auf die Standardausgabe aus.

##Tiefen-Eintauchen

Das Schreiben auf die Standardausgabe ist nicht nur auf Fehlermeldungen beschränkt. Es kann auch für andere Debugging-Zwecke verwendet werden, wie zum Beispiel das Protokollieren bestimmter Variablenwerte oder das Verfolgen der Programmablauf. Allerdings sollte man darauf achten, dass das Schreiben auf die Standardausgabe die Ausführungsgeschwindigkeit des Programms beeinflusst, daher sollte es nur für Debugging-Zwecke und nicht in der Produktionsumgebung verwendet werden.

##Siehe auch

- [Go Dokumentation über fmt.Fprintf()](https://golang.org/pkg/fmt/#Fprintf)
- [Go Dokumentation über io.Writer](https://golang.org/pkg/io/#Writer)
- [Offizielle Go Fehlerbehandlungs-Tutorial](https://blog.golang.org/error-handling-and-go)
- [Tutorial: Debugging in Go mit Visual Studio Code](https://www.calhoun.io/debugging-go-code-with-visual-studio-code/)