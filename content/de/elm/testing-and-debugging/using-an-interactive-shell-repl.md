---
aliases:
- /de/elm/using-an-interactive-shell-repl/
date: 2024-01-26 04:13:23.772953-07:00
description: "Die Read-Eval-Print-Schleife (REPL) ist eine einfache, interaktive Programmierumgebung,\
  \ die einzelne Benutzereingaben annimmt, auswertet und das Ergebnis\u2026"
lastmod: 2024-02-18 23:09:04.783204
model: gpt-4-0125-preview
summary: "Die Read-Eval-Print-Schleife (REPL) ist eine einfache, interaktive Programmierumgebung,\
  \ die einzelne Benutzereingaben annimmt, auswertet und das Ergebnis\u2026"
title: Nutzung einer interaktiven Shell (REPL)
---

{{< edit_this_page >}}

## Was & Warum?
Die Read-Eval-Print-Schleife (REPL) ist eine einfache, interaktive Programmierumgebung, die einzelne Benutzereingaben annimmt, auswertet und das Ergebnis an den Benutzer zurückgibt. Elm-Programmierer nutzen REPL für schnelle Experimente, zum Debuggen oder zum Erlernen der Sprache.

## Wie geht das:
Elm wird nicht mit einem integrierten REPL geliefert. Sie können jedoch `elm repl` von Ihrer Kommandozeile verwenden, um nach der Installation von Elm eine Elm-Sitzung zu starten.

```Elm
> import List exposing (..)
> map (\x -> x * 2) [1, 2, 3, 4]
[2,4,6,8] : List number
```

In dieser Sitzung haben wir nach dem Importieren der List-Funktionen die Zahlen in einer Liste verdoppelt und das Ergebnis sofort erhalten.

## Tiefergehend
Das REPL von Elm kann im Vergleich zu denen einiger anderer Sprachen wie Python oder JavaScript als begrenzt erscheinen, da Elm eine kompilierte Sprache ist, die sich darauf konzentriert, Web-Apps zu produzieren. Historisch gesehen hat sich Elm auf vollständige Anwendungen anstelle von Skripten oder Shell-Interaktionen konzentriert.

Alternativen zum REPL von Elm umfassen `elm-live` und Online-Editoren wie Ellie, in denen Sie Änderungen am Code in Echtzeit in einem Browser sehen können.

Bezüglich der Implementierung kompiliert das Elm REPL Elm-Code-Schnipsel im Hintergrund in JavaScript, was Ihnen erlaubt, Elm interaktiv auszuführen. Dies unterscheidet sich von den REPLs interpretierter Sprachen, die diesen Kompilierungsschritt nicht benötigen. Das Elm REPL ist auch abgespeckt, um die Kernsprache leichtgewichtig und fokussiert zu halten.

## Siehe auch
- Elms offizieller Leitfaden zur Interaktivität: https://guide.elm-lang.org/interop/
- Ellie, ein Online-Spielplatz für Elm: https://ellie-app.com/new
- `elm-live`, ein flexibler Entwicklerserver für Elm: https://www.elm-live.com/
