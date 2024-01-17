---
title:                "Schreiben auf die Standardfehlerausgabe"
html_title:           "Haskell: Schreiben auf die Standardfehlerausgabe"
simple_title:         "Schreiben auf die Standardfehlerausgabe"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Was & Warum?

Wenn du als Programmierer:in eine Fehlermeldung siehst, stammt sie in der Regel aus dem Standardfehler-Ausgabekanal (engl. "Standard Error") deiner Anwendung. Das Schreiben von Nachrichten an diesen Kanal ist ein wichtiger Teil der Fehlerbehandlung und hilft dir dabei, Fehler zu verstehen und zu beheben.

# Wie funktioniert's?

Um eine Nachricht an den Standardfehler-Ausgabekanal zu senden, musst du die Funktion `hPutStrLn` aus dem Modul `System.IO` verwenden. Ein Beispielcode sieht folgendermaßen aus:

```Haskell
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  hPutStrLn stderr "Ups, hier ist ein Fehler passiert."
```

Die `hPutStrLn` Funktion erwartet als erstes Argument eine Handle-Referenz auf den Kanal, an den die Nachricht geschrieben werden soll. Hier verwenden wir `stderr` als Handle-Referenz für den Standardfehler-Ausgabekanal. Als zweites Argument wird die eigentliche Nachricht übergeben.

Beim Ausführen des obigen Codes wird die Nachricht "Ups, hier ist ein Fehler passiert." direkt auf deiner Konsole ausgegeben. Je nach System wird die Nachricht dabei anders formatiert, aber in der Regel wird sie in roter Farbe und/oder in einem abweichenden Schriftstil dargestellt.

# Tiefer ins Detail

Das Konzept des Standardfehler-Ausgabekanals stammt aus den Anfängen des Unix-Betriebssystems und ist ein wichtiger Teil der Unix-Philosophie, Fehler explizit zu behandeln und detaillierte Informationen zur Fehlerursache bereitzustellen.

Alternativ zur Verwendung des Standardfehler-Ausgabekanals kannst du auch Nachrichten über den Standardausgabekanal (engl. "Standard Output") schreiben. Jedoch sollten Fehlermeldungen immer über den Standardfehler-Ausgabekanal ausgegeben werden, um sicherzustellen, dass sie nicht mit anderen Nachrichten vermischt werden und gut sichtbar sind.

Die Implementierung des Standardfehler-Ausgabekanals kann je nach System und Programmiersprache variieren. In Haskell wird der Standardfehler-Ausgabekanal durch das Handle `stderr` dargestellt, das Teil des Standard-Prozesses ist und somit immer verfügbar ist.

# Weitere Informationen

Weitere Informationen zur Funktion `hPutStrLn` und zum Umgang mit Fehlern in Haskell findest du in der offiziellen Dokumentation des Moduls `System.IO` und des Pakets `base`.

[Dokumentation zu System.IO](https://www.haskell.org/ghc/docs/latest/html/libraries/base-4.15.0.0/System-IO.html)
[Dokumentation zu base](https://www.haskell.org/ghc/docs/latest/html/libraries/base-4.15.0.0/)