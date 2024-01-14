---
title:                "Ruby: Schreiben auf Standardfehler"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Warum

Das Schreiben auf den Standardfehler (Standard Error) ist ein wichtiger Teil der Programmierung in Ruby. Es ermöglicht uns, Fehlermeldungen und andere wichtige Informationen während der Ausführung unseres Codes zu überwachen und zu erfassen. In diesem Blogbeitrag werden wir uns genauer anschauen, warum es wichtig ist, auf den Standardfehler zu schreiben.

# So geht's

Das Schreiben auf den Standardfehler ist in Ruby ganz einfach. Wir verwenden einfach den Befehl `STDERR.puts`. Lass uns ein Beispiel anschauen:

```ruby
STDERR.puts "Dies ist eine Fehlermeldung"
```

Wenn wir diesen Code ausführen, wird die Ausgabe auf dem Standardfehler ausgegeben, anstatt auf dem Standardausgang wie üblich. Dies hilft uns, Fehlermeldungen und andere wichtige Informationen sofort zu erkennen.

Eine weitere Möglichkeit, auf den Standardfehler zu schreiben, ist die Verwendung des `raise`-Befehls. Wenn wir `raise` ohne einen Fehler übergeben, wird automatisch eine `RuntimeError` erzeugt und auf den Standardfehler geschrieben.

```ruby
raise "Dies ist eine RuntimeError"
```

Der Output wird ähnlich aussehen wie der Output vom `STDERR.puts`-Befehl.

# Tiefere Einblicke

Das Schreiben auf den Standardfehler ermöglicht es uns, Fehlermeldungen zu erfassen und zu überwachen. Es ist besonders nützlich beim Debuggen von Code oder bei der Entwicklung von komplexen Anwendungen. Indem wir Fehlermeldungen auf den Standardfehler schreiben, können wir sicherstellen, dass sie von anderen Teilen unseres Codes erfasst und verarbeitet werden können.

Darüber hinaus ist es eine gute Praxis, auf den Standardfehler zu schreiben, wenn wir Ausnahmen (Exceptions) erfassen und behandeln. Indem wir die Ausnahme auf den Standardfehler schreiben, stellen wir sicher, dass sie nicht einfach verschluckt wird und wir sie untersuchen können.

Insgesamt ist das Schreiben auf den Standardfehler eine wichtige Fähigkeit für jeden Ruby-Entwickler. Es ermöglicht uns, unseren Code besser zu verstehen und Probleme effektiver zu beheben.

# Siehe auch

- [The Ruby Standard Library Documentation](https://ruby-doc.org/stdlib-2.7.0/libdoc/logger/rdoc/Logger.html)
- [Ruby Exceptions and Exception Handling](https://www.rubyguides.com/2019/05/ruby-exception-handling/)
- [Ruby for Beginners](https://ruby-for-beginners.rubymonstas.org/)