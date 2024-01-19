---
title:                "Eine Textdatei lesen"
html_title:           "Bash: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

---

## Was & Warum?

Das Lesen einer Textdatei ist der Prozess, bei dem ein Programm Daten aus einer externen Datei holt. Programmierer machen das, um Nutzerdaten zu verarbeiten, Ergebnisse zu speichern oder Konfigurationen zu laden.

## So geht's:

In Ruby ist es einfach, eine Textdatei zu lesen. Hier sehen wir, wie das gemacht wird:

```Ruby
datei = File.open("beispiel.txt", "r")

while zeile = datei.gets
  puts zeile
end

datei.close
```

Der Ausdruck `File.open("beispiel.txt", "r")` öffnet die Datei `beispiel.txt` im Lese-Modus ("r"). `datei.gets` liest Zeile für Zeile aus der Datei und `puts zeile` gibt jede Zeile aus.

## Tiefgehende Informationen

Historisch gesehen kam das Lesen von Textdateien mit der Erfindung von persistentem Speicher, also Festplatten und später auch SSDs. Alternativ könnten Sie Netzwerkressourcen oder Datenbanken verwenden, aber das ist eine andere Geschichte.

Unter der Haube ruft Ruby die Basisfunktionen Ihres Betriebssystems auf. Ruby's `File.open` und `gets` sind eigentlich nur nette Umschreibungen für `fopen` und `fgets` in C.

## Siehe auch 

Für weiterführende Informationen empfehlen wir die offizielle [Ruby-Dokumentation](https://ruby-doc.org/core-2.7.0/File.html), den [Pragmatic Programmer's Guide](https://ruby-doc.com/docs/ProgrammingRuby/) und [stackoverflow](https://stackoverflow.com/questions/tagged/ruby-file).

---

The text above is the informal, concise Ruby file-reading tutorial you requested. For further information or clarification, please contact me directly.