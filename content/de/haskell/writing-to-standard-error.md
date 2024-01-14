---
title:                "Haskell: Schreiben auf Standardfehler"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben an den Standardfehler ist eine wichtige Fähigkeit für jeden Haskell Programmierer. Es ermöglicht uns, wichtige Fehlermeldungen anzuzeigen, die uns bei der Fehlersuche und -behebung helfen können.

## Wie man

Das Schreiben an den Standardfehler in Haskell ist einfach. Man kann einfach die `hPutStrLn` Funktion aus dem `System.IO` Modul verwenden, um eine Zeichenfolge an den Standardfehler zu senden.

```Haskell
import System.IO

main = do
    hPutStrLn stderr "Dies ist eine Fehlermeldung"
```

Dies wird die Zeichenfolge "Dies ist eine Fehlermeldung" an den Standardfehler senden. Beachten Sie, dass wir hier `stderr` statt `stdout` verwenden, um spezifisch den Standardfehler anstatt des Standardausgabestroms zu adressieren.

## Tiefgehender Einblick

Es gibt einige wichtige Dinge zu beachten, wenn man an den Standardfehler schreibt. Zum Beispiel bieten viele Bibliotheken wie `Control.Exception` Funktionen wie `throwIO` und `throw` an, die Fehler an den Standardfehler senden. Bei der Verwendung dieser Funktionen müssen wir jedoch vorsichtig sein, um nicht versehentlich wichtige Informationen zu verlieren.

Eine weitere wichtige Sache ist, dass der Standardfehler im Allgemeinen nicht gepuffert wird, was bedeutet, dass jeder Aufruf von `hPutStrLn` sofort die Zeichenfolge an den Standardfehler sendet. Dies kann zu einer unübersichtlichen Ausgabe führen, insbesondere wenn mehrere Threads gleichzeitig auf den Standardfehler schreiben.

## Siehe auch

- [Haskell Dokumentation zu System.IO](https://hackage.haskell.org/package/base/docs/System-IO.html)
- [Fehlerbehandlung in Haskell](https://wiki.haskell.org/Error_handling)