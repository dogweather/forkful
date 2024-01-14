---
title:                "Ruby: Lesen einer Textdatei"
simple_title:         "Lesen einer Textdatei"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von Textdateien ist eine der grundlegenden Funktionen beim Programmieren mit Ruby. Egal ob du Daten aus einer CSV-Datei lesen möchtest oder eine Textdatei als Teil eines Skripts verwenden willst, das Verständnis dieser Funktionalität ist entscheidend. In diesem Beitrag werde ich erklären, warum es wichtig ist, Textdateien in Ruby zu lesen und wie man dies effektiv tun kann.

## Wie man eine Textdatei in Ruby liest

Zum Lesen einer Textdatei in Ruby gibt es mehrere Optionen, aber der einfachste Weg ist die Verwendung der `File`-Klasse. Mit der Methode `::open`, können wir eine Textdatei öffnen und eine Block-Struktur verwenden, um den Code auszuführen, der auf die Datei zugreift:

```Ruby
File.open("textdatei.txt", "r") do |file|
  while line = file.gets
    puts line
  end
end
```

Hier öffnen wir die Textdatei "textdatei.txt" und lesen jede Zeile einzeln aus. Die `file.gets`-Methode gibt die Zeile als String zurück, während `puts` sie auf der Konsole ausgibt. Die `File`-Klasse hat auch andere nützliche Methoden wie `read` oder `readlines`, die dir helfen können, die Datei auf unterschiedliche Weise zu lesen.

## Tiefergehende Erklärung

Beim Lesen einer Textdatei ist es wichtig zu verstehen, dass Ruby die Datei Zeile für Zeile liest. Das bedeutet, dass alle Methoden, die wir auf die Datei anwenden, auf die jeweils aktuelle Zeile angewendet werden, bis wir zur nächsten Zeile springen. Wenn die Datei komplett gelesen wurde, wird `file.gets` `nil` zurückgeben und die Schleife beendet.

Eine wichtige Sache beim Lesen von Textdateien ist auch, die richtige Encoding zu verwenden. Textdateien können in verschiedenen Encodings gespeichert werden, also muss dein Ruby-Code das richtige Encoding kennen, um den Inhalt richtig darzustellen. Glücklicherweise bietet Ruby hierfür verschiedene Methoden, die du auf die `file`-Variable anwenden kannst, um Informationen über die Datei zu erhalten, z.B `file.external_encoding` oder `file.internal_encoding`.

## Siehe auch

- [Ruby Dokumentation zu File](https://ruby-doc.org/core-2.7.3/File.html)
- [Erlangung von Ruby Datei-Informationen mithilfe von File](https://www.rubyguides.com/2015/05/working-with-files-ruby/)