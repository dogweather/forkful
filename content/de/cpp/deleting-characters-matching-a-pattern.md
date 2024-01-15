---
title:                "Beseitigung von Zeichen, die einem Muster entsprechen"
html_title:           "C++: Beseitigung von Zeichen, die einem Muster entsprechen"
simple_title:         "Beseitigung von Zeichen, die einem Muster entsprechen"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Es kann Situationen geben, in denen es notwendig ist, bestimmte Zeichen aus Strings oder Textdateien zu entfernen. Dies kann zum Beispiel erforderlich sein, um unerwünschte Sonderzeichen oder Leerzeichen zu löschen, um die String-Manipulation zu erleichtern.

## Wie geht man vor?

Um Zeichen zu löschen, die einem bestimmten Muster entsprechen, können wir die Funktion `std::remove_if` aus der Standardbibliothek `<algorithm>` verwenden. Sie akzeptiert einen Iterator auf den Anfang und das Ende des Bereichs, in dem die Zeichen gelöscht werden sollen, sowie ein Prädikat, das das Muster definiert. Alternativ können wir auch eine For-Schleife verwenden und jedes Zeichen im String überprüfen, ob es dem Muster entspricht und es dann löschen.

```C++
std::string text = "Dies ist ein Text mit einigen Satzzeichen!";
text.erase(std::remove_if(text.begin(), text.end(), [](char c){ return c == ',' || c == '!'; }), text.end());
// text enthält nun "Dies ist ein Text mit einigen Satzzeichen"
```

## Tiefer eintauchen

Die Funktion `std::remove_if` arbeitet nach dem Prinzip "Zurückgeben, was behalten werden soll". Es verschiebt alle Elemente im Bereich, die dem Prädikat nicht entsprechen, an den Anfang und gibt einen Iterator auf das erste Element nach dem neuen Ende zurück. Das Endresultat ist also ein Bereich, der die gewünschten Zeichen enthält, gefolgt von Platzhaltern für die entfernten Zeichen. Diese Platzhalter können dann mit der Funktion `resize` des `std::string`-Objekts entfernt werden.

Um weitere Informationen darüber zu erfahren, wie die Funktion `remove_if` funktioniert und wie man sie effektiv einsetzen kann, empfehlen wir die offizielle Dokumentation der C++ Standardbibliothek.

## Siehe auch

- [std::remove_if Dokumentation](https://en.cppreference.com/w/cpp/algorithm/remove)
- [std::string Dokumentation](https://en.cppreference.com/w/cpp/string/basic_string)
- [std::remove_if Beispielcode](https://www.geeksforgeeks.org/remove-characters-from-a-string-that-appear-consecutively/)