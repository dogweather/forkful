---
title:                "Die Verwendung von regulären Ausdrücken"
html_title:           "Elm: Die Verwendung von regulären Ausdrücken"
simple_title:         "Die Verwendung von regulären Ausdrücken"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Reguläre Ausdrücke sind eine nützliche Funktion in Elm, die es ermöglicht, Textmuster in einer effizienten und genauen Weise zu durchsuchen und zu bearbeiten. Durch die Verwendung von regulären Ausdrücken können Entwickler Zeit sparen und komplexe Aufgaben effektiv lösen.

## Wie geht man vor?

Die Verwendung von regulären Ausdrücken in Elm ist einfach und unkompliziert. Zunächst müssen Sie das Paket `elm/regex` installieren, indem Sie den Befehl `elm install elm/regex` in der Befehlszeile eingeben.

Um einen regulären Ausdruck zu erstellen, müssen Sie den `Regex`-Befehl verwenden und diesen mit einem Muster als String-Parameter versehen. Zum Beispiel:

```Elm
Regex.fromRegex "(foo|bar)\\d+"
```

In diesem Beispiel wird ein regulärer Ausdruck erstellt, der entweder das Wort "foo" oder "bar" gefolgt von einer beliebigen Anzahl von Ziffern erkennt.

Um eine Zeichenfolge mit Hilfe eines regulären Ausdrucks zu überprüfen, können Sie die Funktion `Regex.match` verwenden. Diese Funktion erwartet zwei Parameter: den regulären Ausdruck und die zu überprüfende Zeichenfolge. Zum Beispiel:

```Elm
Regex.match (Regex.fromRegex "a*b") "aaab"
```

Dieser Code prüft, ob die Zeichenfolge "aaab" dem angegebenen regulären Ausdruck entspricht, und gibt `Just true` zurück, da dies der Fall ist.

## Tiefer Einblick

Reguläre Ausdrücke können auch verwendet werden, um in einer Zeichenfolge Veränderungen vorzunehmen. Dazu können Sie die Funktion `Regex.replace` verwenden, die ähnlich wie `Regex.match` zwei Parameter erwartet: den regulären Ausdruck und die zu bearbeitende Zeichenfolge.

Zusätzlich können Sie mithilfe von regulären Ausdrücken auch Gruppen definieren, die in der bearbeiteten Zeichenfolge verändert werden sollen. Dies ermöglicht es, Textmuster effizient zu ersetzen und zu manipulieren.

Weitere Informationen und Beispiele für die Verwendung von regulären Ausdrücken in Elm finden Sie in der offiziellen Dokumentation des `elm/regex`-Pakets [hier](https://package.elm-lang.org/packages/elm/regex/latest/).

## Siehe auch

- [Offizielle Dokumentation des `elm/regex`-Pakets](https://package.elm-lang.org/packages/elm/regex/latest/)
- [RegExr - Interaktiver RegEx-Tester](https://regexr.com/)