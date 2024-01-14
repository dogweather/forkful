---
title:                "Ruby: Eine Datumsangabe in eine Zeichenfolge umwandeln"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

### Warum

Das Konvertieren eines Datums in einen String kann eine nützliche Fähigkeit für jeden Ruby-Programmierer sein. Sie können es verwenden, um Datumstempel in ein bestimmtes Format für Anzeigezwecke zu verwandeln oder es als Teil eines größeren Datumsmanipulationsprozesses zu verwenden. In diesem Blog-Beitrag werde ich Ihnen zeigen, wie Sie ein Datum in einen String konvertieren können und einige tiefergehende Konzepte beleuchten.

### Wie geht es?

Das Konvertieren eines Datums in einen String in Ruby ist relativ einfach und kann auf verschiedene Arten erfolgen. Hier sind zwei Möglichkeiten, dies zu erreichen.

```Ruby
# Beispiel 1: Verwendung der strftime-Methode
date = Time.now # Aktuelles Datum und Uhrzeit
puts date.strftime("%m/%d/%Y") # Ausgabe: 09/20/2021

# Beispiel 2: Verwendung der to_s-Methode
date = Date.today # Aktuelles Datum
puts date.to_s # Ausgabe: 2021-09-20
```

Wie Sie sehen können, können Sie entweder die strftime-Methode mit einem bestimmten Format verwenden oder einfach die to_s-Methode verwenden, um das Datum in ein Standardformat zu konvertieren. Es gibt auch andere Methoden und Optionen, die Sie ausprobieren können, um das Datum in verschiedene Formate zu konvertieren.

### Tiefes Eintauchen

Bei der Konvertierung eines Datums in einen String gibt es einige tiefergehende Konzepte zu beachten. Zum Beispiel können Sie durch die Verwendung von Internationalisierungsgemäßen und gem-Dateien die Standardformate ändern oder benutzerdefinierte Formate erstellen. Sie können auch die verschiedenen Formate verwenden, die von strftime unterstützt werden, um das Datum in verschiedenen Sprachen anzuzeigen.

Ein weiteres wichtiges Konzept beim Konvertieren eines Datums in einen String ist die Zeitzone. Ruby bietet eine Reihe von Methoden, mit denen Sie die Zeitzone des Datums ändern oder in Bezug auf eine bestimmte Zeitzone konvertieren können.

Um noch tiefer zu gehen, können Sie verschiedene Ruby-Gem-Dateien erkunden, die speziell für die Datums- und Zeitmanipulation entwickelt wurden und die Ihnen noch mehr Funktionen und Flexibilität bieten können.

### Siehe Auch

Hier sind einige nützliche Links, die Ihnen helfen können, Ihre Fähigkeiten bei der Konvertierung von Datum zu vertiefen:

- [Ruby-Dokumentation zur strftime-Methode](https://ruby-doc.org/core-3.0.0/DateTime.html#method-i-strftime)
- [Ruby-Dokumentation zur to_s-Methode](https://ruby-doc.org/core-3.0.0/DateTime.html#method-i-to_s)
- [Ruby on Rails Datums- und Zeitmanipulations-Gem](https://github.com/rails/rails/tree/master/activesupport)
- [Internationalisierungsgem für Ruby](https://github.com/svenfuchs/i18n)

Mit diesen Informationen sollten Sie jetzt in der Lage sein, problemlos ein Datum in einen String umzuwandeln und die verschiedenen Aspekte dieses Prozesses zu verstehen. Viel Spaß beim Programmieren mit Ruby!