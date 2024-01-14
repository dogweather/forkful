---
title:    "Ruby: Schreiben auf Standardfehler"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum
Warum sollte jemand Zeit damit verbringen, in das standard error zu schreiben? Die einfache Antwort: um Fehlermeldungen und Debugging-Informationen zu verbessern.

## Wie
Es gibt verschiedene Möglichkeiten, in Ruby in das standard error zu schreiben. Eine Möglichkeit ist der Befehl `STDERR.puts`, der die angegebene Nachricht in das standard error ausgibt. Zum Beispiel:

```Ruby
STDERR.puts "Dies ist eine Nachricht für das standard error"
```

Dies würde folgende Ausgabe erzeugen:

`Dies ist eine Nachricht für das standard error`

Eine weitere Möglichkeit ist es, den Befehl `STDERR.write` zu nutzen, der die Nachricht, ähnlich wie `puts`, in das standard error ausgibt. Zum Beispiel:

```Ruby
STDERR.write "Dies ist eine andere Nachricht für das standard error"
```

Dies würde folgende Ausgabe erzeugen:

`Dies ist eine andere Nachricht für das standard error`

## Deep Dive
Wenn wir in das standard error schreiben, können wir erweiterte Debugging-Informationen anzeigen oder gezielt auf bestimmte Fehler hinweisen. Zum Beispiel könnte man in einem Ruby-Programm eine Fehlermeldung zeigen, wenn eine bestimmte Bedingung nicht erfüllt wird. Dies könnte so aussehen:

```Ruby
if user_input != expected_input
  STDERR.puts "Fehler: Das Benutzer-Input stimmt nicht mit dem erwarteten Input überein."
end
```

Durch das Schreiben in das standard error können wir also mehr Kontrolle über die Ausgabe von Fehlermeldungen und zusätzlichen Informationen haben.

## Siehe Auch
- [Ruby Dokumentation über STDERR](https://ruby-doc.org/core-2.7.1/IO.html#method-c-new-label-Standard+Streams)
- [Artikel über das Schreiben in das standard error in Ruby](https://quickleft.com/blog/ruby-stdin-stdout-stderr/)