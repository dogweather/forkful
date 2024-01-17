---
title:                "Schreiben auf standard error"
html_title:           "Ruby: Schreiben auf standard error"
simple_title:         "Schreiben auf standard error"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Schreiben von Standardfehlermeldungen bedeutet, dass Programmierer ihre Fehlermeldungen auf einem unverzichtbaren Ausgabekanal festlegen. Dies ist hilfreich, um Fehlermeldungen von normalen Ausgaben zu unterscheiden und um sicherzustellen, dass wichtige Fehler nicht übersehen werden.

## Wie geht das?
Die Syntax zum Schreiben von Standardfehlern in Ruby ist einfach - einfach ```$stderr.puts "Fehlermeldung" ``` verwenden. Dies wird den Fehler in der Kommandozeile ausgeben, unabhängig davon, wo die normale Ausgabe erfolgt.

Beispiel:
```Ruby
$stderr.puts "Es ist ein Fehler aufgetreten."
```

Ausgabe:
```
Es ist ein Fehler aufgetreten.
```

## Tief einsteigen
Das Schreiben von Standardfehlern hat seinen Ursprung in den frühen Tagen der Programmierung, als die Ausgabe normalerweise in die Kommandozeile geschrieben wurde. Alternativen zum Schreiben von Standardfehlern sind das Schreiben in eine Datei oder das Senden per E-Mail. Es ist jedoch immer noch eine gängige Praxis, da es einfach und effektiv ist.

Eine wichtige Sache zu beachten ist, dass das Schreiben von Standardfehlern auch die Leistung beeinflussen kann. Zu viele Fehlermeldungen auf dem Standardfehlerkanal können die gesamte Programmleistung verlangsamen.

## Siehe auch
Hier sind einige hilfreiche Quellen, um mehr über das Schreiben von Standardfehlern in Ruby zu erfahren:

- Offizielle Ruby-Dokumentation: https://ruby-doc.org/core-2.7.0/IO.html#method-i-puts
- Medium Artikel von Michal Lipski: https://medium.com/@mlipski87/standard-output-vs-standard-error-a2298a6a23ba
- Ruby Guides: https://www.rubyguides.com/2015/03/ruby-standard-error/