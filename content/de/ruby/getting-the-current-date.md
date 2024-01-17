---
title:                "Das aktuelle Datum abrufen."
html_title:           "Ruby: Das aktuelle Datum abrufen."
simple_title:         "Das aktuelle Datum abrufen."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Abrufen des aktuellen Datums ist eine gängige Aufgabe für Programmierer in Ruby. Es ermöglicht ihnen, das aktuelle Datum und die aktuelle Zeit für verschiedene Zwecke zu nutzen, wie zum Beispiel zur Verfolgung der Ausführungszeit eines Programms oder zur Anzeige des aktuellen Datums in einem Benutzerinterface. Es ist eine einfache, aber nützliche Funktion, die in vielen Programmen verwendet wird.

## Wie geht's?

Die aktuelle Datumsfunktion in Ruby ist ```Date.today```. Sie gibt das heutige Datum als Objekt des Typs ```Date``` zurück. Hier ist ein Beispielcode, der das aktuelle Datum abruft und es in einer Variablen speichert, um es später zu verwenden:

```Ruby
current_date = Date.today
puts current_date
```

Die Ausgabe dieses Codes wäre das aktuelle Datum in folgendem Format: Jahr-Monat-Tag (z.B. 2021-08-12).

## Tiefentauchgang

#### Historischer Kontext
Die Verwendung des aktuellen Datums ist ein wichtiger Teil der Programmierung. Früher mussten Entwickler komplexe Berechnungen durchführen, um das aktuelle Datum zu erhalten. Mit der Einführung von Ruby und anderen Programmiersprachen ist es nun viel einfacher, das aktuelle Datum zu erhalten.

#### Alternativen
Während ```Date.today``` die gängigste Methode ist, um das aktuelle Datum in Ruby zu erhalten, gibt es auch andere Möglichkeiten, dies zu tun. Zum Beispiel können Entwickler die ```Time.now``` Funktion verwenden, die das aktuelle Datum und die aktuelle Zeit zurückgibt. Es ist wichtig zu beachten, dass diese Funktion die Zeitzone des Systems verwendet, auf dem das Programm ausgeführt wird.

#### Implementierungsdetails
Die Standardbibliothek von Ruby enthält die Klasse ```Date``` und die Methode ```today```, um das aktuelle Datum abzurufen. Diese Methode ruft das Datum aus dem Betriebssystem des Computers ab. Es ist auch möglich, das Datum mit benutzerdefinierten Formatierungsoptionen abzurufen, indem man die Methode ```strftime``` verwendet.

## Siehe auch

Weitere Informationen zum Abrufen des aktuellen Datums in Ruby finden Sie in der offiziellen Dokumentation unter [https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html#method-i-today](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html#method-i-today).

Weitere Möglichkeiten, das aktuelle Datum in Ruby zu erhalten, finden Sie in diesem nützlichen Artikel: [https://www.rubyguides.com/2015/05/working-with-dates-ruby/](https://www.rubyguides.com/2015/05/working-with-dates-ruby/).