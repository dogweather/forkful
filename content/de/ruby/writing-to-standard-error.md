---
title:    "Ruby: Schreiben auf Standardfehler"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

# Warum

Das Schreiben in die Standardfehlerausgabe ist ein wichtiger Bestandteil der Ruby-Programmierung, der oft übersehen wird. Es ermöglicht die Fehlerausgabe und die Diagnose von Fehlern auf einer tieferen Ebene, als es mit der Standardausgabe möglich ist.

# Wie man

Um in die Standardfehlerausgabe zu schreiben, gibt es in Ruby die `STDERR`-Konstante. Mit der `puts`-Methode können wir einfach eine Nachricht in die Standardfehlerausgabe schreiben.

```Ruby
STDERR.puts "Dies ist eine Fehlermeldung"
```

Dies wird eine Ausgabe wie folgt erzeugen:

```
Dies ist eine Fehlermeldung
```

Ein weiteres nützliches Werkzeug ist die `warn`-Methode, die eine Warnung in die Standardfehlerausgabe schreibt.

```Ruby
warn "Achtung: Dies ist eine Warnung"
```

Das Ergebnis sieht so aus:

```
Achtung: Dies ist eine Warnung
```

Es ist auch möglich, mehrere Argumente an `puts` oder `warn` zu übergeben, die dann durch Leerzeichen getrennt ausgegeben werden.

```Ruby
STDERR.puts "Dieser", "Text", "wird", "getrennt"
```

Dieses Beispiel würde eine Ausgabe erzeugen:

```
Dieser Text wird getrennt
```

# Tiefer Einblick

Neben der Ausgabe von Fehlern und Warnungen gibt es noch einige andere Möglichkeiten, wie das Schreiben in die Standardfehlerausgabe hilfreich sein kann. Eine davon ist die Verwendung von `raise` zusammen mit der `rescue`-Klausel. Hier ein Beispiel:

```Ruby
begin
  # irgendein fehlerhafter Code
rescue => e
  STDERR.puts "Es ist ein Fehler aufgetreten: #{e.message}"
end
```

In diesem Beispiel fangen wir einen Fehler ab und schreiben die Fehlermeldung in die Standardfehlerausgabe. Dies ermöglicht eine detailliertere Fehlerbehandlung und Diagnose.

Ein weiterer interessanter Aspekt ist, dass die Ausgabe in der Standardfehlerausgabe auch gut für die Protokollierung von Ereignissen oder Debugging-Zwecken verwendet werden kann. Indem wir gezielt bestimmte Nachrichten in die Standardfehlerausgabe schreiben, können wir den Ablauf des Programms besser verfolgen und potenzielle Fehler aufspüren.

# Siehe auch

- [Offizielle Ruby-Dokumentation zur Standardfehlerausgabe](https://ruby-doc.org/core-2.6.1/IO.html#method-c-new-label-Standard+Streams)
- [Artikel zur Fehlerbehandlung in Ruby](https://www.rubyguides.com/2019/04/ruby-exceptions/)
- [RubyLogger auf GitHub](https://github.com/ruby/logger)