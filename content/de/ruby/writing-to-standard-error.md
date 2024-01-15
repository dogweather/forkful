---
title:                "Schreiben auf Standardfehler"
html_title:           "Ruby: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich überhaupt damit beschäftigen, Fehlermeldungen an den Standardfehlerausgabestrom zu schreiben? Ganz einfach - es ist eine effektive Möglichkeit, Fehler in Ruby-Programmen zu diagnostizieren und zu beheben. Durch das Schreiben von Fehlermeldungen an den Standardfehlerausgabestrom erhalten Sie detaillierte Informationen über den Programmablauf und können so Probleme schneller und präziser lösen.

## Wie geht das?

Das Schreiben von Fehlermeldungen an den Standardfehlerausgabestrom in Ruby ist denkbar einfach. Alles was Sie tun müssen, ist das `STDERR`-Objekt zu verwenden und die `puts`-Methode aufzurufen, um Ihre Meldung auszugeben. Hier ist ein Beispiel:

```Ruby
STDERR.puts "Ein Fehler ist aufgetreten: Benutzername konnte nicht gefunden werden."
```

Dies wird einen Fehler im Standardfehlerausgabestrom ausgeben, der von anderen Ausgaben im Programm unterschieden werden kann. Hier ist ein Beispiel für die Ausgabe dieses Codes:

```
$ ruby fehler.rb
Ein Fehler ist aufgetreten: Benutzername konnte nicht gefunden werden.
```

Wie Sie sehen können, wird die Fehlermeldung als separate Zeile ausgegeben und gibt Ihnen wichtige Informationen darüber, was schief gelaufen ist.

## Tiefergehende Informationen

Das `STDERR`-Objekt ist ein Teil der Standardbibliothek von Ruby und wird verwendet, um Fehlermeldungen und andere wichtige Nachrichten an den Standardfehlerausgabestrom zu senden. Es ist wichtig zu beachten, dass das Schreiben zu `STDERR` nicht das gleiche ist wie das Schreiben zu `STDOUT`, dem Standardausgabestrom.

Im Allgemeinen wird empfohlen, Fehlermeldungen an den Standardfehlerausgabestrom zu schreiben, da er unabhängig vom Standardausgabestrom gehandhabt wird und somit wichtige Informationen nicht vom Programmoutput überschrieben werden.

Eine weitere Möglichkeit, auf `STDERR` zuzugreifen, ist die Verwendung der `warn`-Methode. Diese Methode kann verwendet werden, um Warnungen und nicht-kritische Fehler an den Standardfehlerausgabestrom zu senden. Hier ist ein Beispiel:

```Ruby
warn "Benutzername ist zu kurz."
```

Dies wird eine Warnung im Standardfehlerausgabestrom ausgeben, die von anderen Ausgaben unterscheidbar ist.

Es ist auch möglich, eigene benutzerdefinierte Klassen zu erstellen, die von `Exception` erben und dann mithilfe der `raise`-Methode Fehlermeldungen an `STDERR` senden können.

## Siehe auch

- [Ruby Dokumentation zu STDERR](https://ruby-doc.org/core-3.0.2/STDERR.html)
- [Ruby Dokumentation zu Exception](https://ruby-doc.org/core-3.0.2/Exception.html)
- [Artikel über die Verwendung von STDERR in Ruby](https://www.rubyguides.com/2019/05/ruby-io/)