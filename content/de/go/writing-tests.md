---
title:                "Go: Tests schreiben"
programming_language: "Go"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man Tests schreiben? Ist es nicht genug, dass unser Code einfach funktioniert?

Die Antwort ist einfach: Tests helfen uns dabei, sicherzustellen, dass unser Code nicht nur funktioniert, sondern auch weiterhin funktioniert. Sie geben uns die Zuversicht, Änderungen an unserem Code vorzunehmen, ohne befürchten zu müssen, dass wir etwas kaputt machen.

## Wie
Um Tests in Go zu schreiben, müssen wir zunächst das standardmäßige Test-Framework von Go importieren. Dies geschieht mit dem folgenden Import-Statement:

```Go
import "testing"
```

Um eine neue Test-Funktion zu erstellen, verwenden wir das ```func``` Schlüsselwort gefolgt vom namen unserer Funktion und der Parameterliste. Wir definieren auch einen ```*testing.T``` Parameter, der uns beim Schreiben von Tests helfen wird. Ein Beispiel dafür könnte so aussehen:

```Go
func TestAddition(t *testing.T) {
    // Hier schreiben wir unseren Test-Code
}
```

In unserem Test können wir nun die Funktion ```t.Errorf``` verwenden, um Fehlermeldungen auszugeben, falls unser Test fehlschlägt. Es ist auch möglich, mehrere Tests innerhalb einer Funktion zu schreiben, aber es ist eine bewährte Praxis, für jeden Test eine eigene Funktion zu erstellen.

## Deep Dive
Jetzt, da wir wissen, wie wir Tests schreiben können, wollen wir uns einige bewährte Praktiken für das Schreiben von effektiven Tests ansehen.

1. Behalten Sie Ihre Tests immer im gleichen Paket wie Ihre Code-Dateien. Dies hilft dabei, alles organisiert und leicht zugänglich zu halten.

2. Verwenden Sie aussagekräftige Test-Namen. Vermeiden Sie es, Namen wie ```test1``` oder ```myTest``` zu verwenden. Verwenden Sie stattdessen einen Namen, der beschreibt, was genau in diesem Test überprüft wird.

3. Testen Sie sowohl den Erfolg als auch das Fehlschlagen von Funktionen. Dies stellt sicher, dass unser Code fehlerfrei funktioniert und auch die richtigen Fehlermeldungen ausgibt, falls etwas schief geht.

## Siehe auch
- Offizielle Dokumentation zu Tests in Go: https://golang.org/pkg/testing/
- Ein praktisches Beispiel für das Schreiben von Tests in Go: https://www.calhoun.io/writing-very-large-go-applications-in-a-team/
- Weitere bewährte Praktiken für das Schreiben von Tests in Go: https://blog.alexellis.io/golang-writing-unit-tests/