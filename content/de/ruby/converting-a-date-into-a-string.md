---
title:                "Ein Datum in einen String umwandeln"
html_title:           "Ruby: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Was & Warum?

Die Umwandlung eines Datums in einen String ist eine häufige Aufgabe in der Programmierung, insbesondere in Ruby. Ein Datum ist eine spezifische Datenstruktur und ein String ist eine Textdarstellung, daher kann es nützlich sein, ein Datum in einen String zu konvertieren, um es leichter lesen und manipulieren zu können.

# Wie geht's?

Die Umwandlung eines Datums in einen String ist in Ruby einfach. Hier sind einige Beispiele:

```Ruby
date = Time.new(2021, 05, 08) # Erstellt ein Datum-Objekt
date.to_s # Konvertiert es in einen String: "2021-05-08 00:00:00 +0100"
date.strftime("%d.%m.%Y") # Verwendet die strftime-Methode, um das Datum in einem benutzerdefinierten Format zu konvertieren: "08.05.2021"
```

# Tief tauchen

Die Notwendigkeit, ein Datum in einen String zu konvertieren, stammt aus der Geschichte der Programmierung und den verschiedenen Datenstrukturen, die verwendet werden. Es gibt auch alternative Möglichkeiten, ein Datum darzustellen, wie z.B. mithilfe von Intervallen oder Wochentagsnamen. Die Implementierung der String-Konvertierung für ein Datum hängt von der verwendeten Programmiersprache und Datenstruktur ab.

# Siehe auch

Weitere Informationen zur Formatierung von Datumswerten in Ruby finden Sie in der offiziellen Dokumentation: https://ruby-doc.org/core-3.0.1/Time.html#method-i-strftime