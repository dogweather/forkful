---
title:                "Gleam: Verwendung regulärer Ausdrücke"
simple_title:         "Verwendung regulärer Ausdrücke"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Regex spielt eine wichtige Rolle in der modernen Programmierung und ermöglicht es Entwicklern, komplexe Muster in Texten zu suchen und zu manipulieren. Durch die Verwendung von regulären Ausdrücken können Programmierer effizienter, effektiver und fehlerfreier arbeiten.

## Wie man Regex in Gleam verwendet

Um reguläre Ausdrücke in Gleam zu verwenden, müssen Sie das `regex`-Paket in Ihrem Projekt importieren. Anschließend können Sie die `re`-Funktion verwenden, um einen regulären Ausdruck zu erstellen und dann `matches` aufzurufen, um Übereinstimmungen in einem Text zu suchen. In dem folgenden Beispiel suchen wir nach einer Telefonnummer im Format `xxx-xxx-xxxx` und geben die Übereinstimmung zurück.

```
Gleam import regex

let regex = regex.re("d{3}-d{3}-d{4}")

println(regex.matches("Meine Nummer ist 123-456-7890"))     // Ergebnis: Some(123-456-7890)
```

## Tiefergehende Informationen

Reguläre Ausdrücke in Gleam können auch Gruppierungen, Wiederholungen, Zeichenklassen und vieles mehr enthalten. Sie können auch verwendet werden, um Text zu ersetzen, anstatt nur Übereinstimmungen zu finden. Es gibt viele Ressourcen online, die Ihnen helfen können, mehr über die Verwendung von regulären Ausdrücken in Gleam zu erfahren.

## Siehe auch

- [Gleam Paket: regex](https://github.com/gleam-lang/regex)
- [Offizielle Gleam Dokumentation - Regex](https://gleam.run/book/stdlib/regex.html)
- [Regex Cheat Sheet für Gleam](https://www.debuggex.com/cheatsheet/regex/gleam)