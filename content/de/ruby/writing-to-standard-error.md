---
title:                "Schreiben auf Standardfehler"
html_title:           "Arduino: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Was & Warum?

Schreiben auf Standard Error (stderr) bedeutet, Fehlermeldungen und Diagnostik separat zur Standardausgabe zu senden. Dies hilft dabei, normale Programmausgaben von Fehlermeldungen zu unterscheiden, was vor allem bei der Weiterverarbeitung von Programmausgaben nützlich ist.

## How to:

```Ruby
# Schreiben auf Standard Output (stdout)
puts "Das ist eine normale Ausgabe."

# Schreiben auf Standard Error (stderr)
$stderr.puts "Das ist eine Fehlermeldung."

# Kurzform für Schreiben auf stderr
STDERR.puts "Das ist auch eine Fehlermeldung."
```

Beispiel-Ausgabe (im Terminal):

```
Das ist eine normale Ausgabe.
Das ist eine Fehlermeldung.
Das ist auch eine Fehlermeldung.
```

Nutze Umleitung im Terminal, um die Ausgaben zu trennen:

```
ruby script.rb > ausgabe.txt 2> errorlog.txt
```

Dies schreibt normale Ausgaben in `ausgabe.txt` und Fehlermeldungen in `errorlog.txt`.

## Deep Dive

Schreiben auf stderr ist eine Konvention, die von Unix übernommen wurde, wo `stdout` und `stderr` verschiedene Streams sind. Alternativen zu `$stderr.puts` könnten die Verwendung von `warn` oder das Logging mit einem Bibliotheks-Logger sein. Die Implementierungsdetails sind wichtig: `STDOUT` und `STDERR` sind globale Konstanten in Ruby, die Objekte der Klasse `IO` darstellen und für gewöhnliche bzw. Fehlerausgaben dienen.

## See Also

- Ruby-Dokumentation zu IO: [https://ruby-doc.org/core-3.1.0/IO.html](https://ruby-doc.org/core-3.1.0/IO.html)
- POSIX-Standard für stderr: [https://pubs.opengroup.org/onlinepubs/9699919799/functions/stdin.html](https://pubs.opengroup.org/onlinepubs/9699919799/functions/stdin.html)
- Erläuterung von stdout und stderr in Unix: [https://en.wikipedia.org/wiki/Standard_streams](https://en.wikipedia.org/wiki/Standard_streams)