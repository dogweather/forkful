---
title:    "Haskell: Schreiben auf standardmäßigen Fehler"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Warum

Das Schreiben in die Standardfehlerausgabe (stderr) ist eine ziemlich nützliche Fähigkeit beim Schreiben von Haskell-Programmen. Es ermöglicht uns, Fehlermeldungen oder andere wichtige Informationen während der Ausführung des Programms in einem separaten Kanal zu verfolgen. Dadurch können wir Bugs schneller finden und unseren Code effizienter debuggen.

## Wie man es macht

Die Verwendung von `hPutStrLn` aus der `System.IO`-Bibliothek ist der einfachste Weg, um in die Standardfehlerausgabe zu schreiben. Wir können es verwenden, um einen String zu übergeben, der dann in der Konsole ausgegeben wird.

```Haskell
import System.IO
hPutStrLn stderr "Dies ist eine Fehlermeldung."
```

Das obige Beispiel gibt den String "Dies ist eine Fehlermeldung." in die Standardfehlerausgabe aus. Beachten Sie jedoch, dass wir die Funktion `catch` aus der `Control.Exception`-Bibliothek verwenden sollten, um die Ausführung des Programms bei einem Fehler nicht zu unterbrechen.

## Tief eintauchen

Die Standardfehlerausgabe kann auch verwendet werden, um Fehler und Debugging-Informationen in einer produktiven Umgebung zu protokollieren. Dies ist besonders nützlich, wenn wir unsere Anwendung auf einem Server ausführen, wo wir möglicherweise keinen sofortigen Zugriff auf die Konsole haben.

Eine weitere hilfreiche Funktion ist `hPutChar`, die wir verwenden können, um einzelne Zeichen in die Standardfehlerausgabe zu schreiben. Dies ermöglicht uns, den Fortschritt einer lange andauernden Operation in Echtzeit zu verfolgen.

```Haskell
import System.IO
  
writeProgress :: IO ()
writeProgress = do
  hPutChar stderr '.'
  hFlush stderr
  
main :: IO ()
main = do
  -- Aufruf einer langen und zeitaufwändigen Funktion
  -- und Verfolgung des Fortschritts
  writeProgress -- jedes Mal, wenn ein Schritt abgeschlossen ist
```

## Siehe auch

- [Offizielle Dokumentation zu System.IO](https://hackage.haskell.org/package/base/docs/System-IO.html)
- [Tutorial zu Ausnahmen und Fehlerbehandlung in Haskell](https://wiki.haskell.org/Exception)
- [Der Codewalkthrough-Kanal auf YouTube](https://www.youtube.com/playlist?list=PLFb75JbVWC0EsWqN3EQhdu8J5waC_nrHh), um mehr über fortgeschrittenere Techniken des Schreibens in die Standardfehlerausgabe zu erfahren.