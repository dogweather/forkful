---
title:                "Swift: Schreiben auf den Standardfehler"
simple_title:         "Schreiben auf den Standardfehler"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben von Fehlermeldungen in der Swift-Programmierung kann hilfreich sein, um Fehler in Ihrem Code zu finden und zu beheben. Es ist eine effektive Möglichkeit, Ihre Fehlerbehebungsstrategie zu verbessern und die Qualität Ihrer Anwendungen zu erhöhen.

## Wie geht das?

Um Fehler in Swift zu schreiben, können Sie die print()-Funktion mit der standard error stream-Option verwenden. Dies ermöglicht es Ihnen, eine Fehlermeldung in rot auf der Konsole auszugeben, so dass sie leicht von anderen Ausgaben zu unterscheiden ist. Das Folgende ist ein Beispielcode.

```Swift

func throwError() {

    let errorMessage = "Ein Fehler ist aufgetreten."

    print(errorMessage, to: &stderr)

}

```

Ausgabe:

`Ein Fehler ist aufgetreten.`

Um die Fehlermeldung in rot zu sehen, müssen Sie möglicherweise den Xcode-Debugger aktivieren oder die Ausgabe in einem Terminalfenster betrachten.

## Tiefer gehend

Schreiben von Fehlern in Swift ist eine effektive Möglichkeit, um die Fehlerbehebung in Ihrer Anwendung zu verbessern. Sie können auch benutzerdefinierte Fehlermeldungen für spezifische Situationen erstellen, indem Sie die `Error`-Klasse verwenden. Mit benutzerdefinierten Fehlermeldungen können Sie spezifische Details über einen Fehler angeben und besser verstehen, wo genau der Fehler auftritt.

## Siehe auch

- [Offizielle Swift-Dokumentation über das Schreiben von Fehlermeldungen](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
- [Ein ausführliches Tutorial zum Schreiben von benutzerdefinierten Fehlermeldungen in Swift](https://www.hackingwithswift.com/example-code/language/how-to-create-custom-errors-using-enum)
- [Eine Diskussion in der Swift-Community über die Verwendung von print() vs. debugPrint() für Fehlermeldungen](https://forums.swift.org/t/print-vs-debugprint-for-error-messages/6042)