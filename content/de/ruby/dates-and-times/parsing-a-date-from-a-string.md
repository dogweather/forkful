---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:10.162738-07:00
description: "Wie: In Ruby bietet die Standardbibliothek direkte Wege, um Daten aus\
  \ Strings mithilfe der Klassen `Date` und `DateTime` zu parsen. Hier ist, wie man\
  \ es\u2026"
lastmod: '2024-03-13T22:44:54.409701-06:00'
model: gpt-4-0125-preview
summary: In Ruby bietet die Standardbibliothek direkte Wege, um Daten aus Strings
  mithilfe der Klassen `Date` und `DateTime` zu parsen.
title: Einen Datum aus einem String analysieren
weight: 30
---

## Wie:
In Ruby bietet die Standardbibliothek direkte Wege, um Daten aus Strings mithilfe der Klassen `Date` und `DateTime` zu parsen. Hier ist, wie man es mit Rubys integrierten Methoden macht:

```ruby
require 'date'

# Ein Datum aus einem String parsen
date_string = "2023-04-01"
parsed_date = Date.parse(date_string)
puts parsed_date
# => 2023-04-01

# DateTime für eine detailliertere Zeitdarstellung
datetime_string = "2023-04-01T15:30:45+00:00"
parsed_datetime = DateTime.parse(datetime_string)
puts parsed_datetime
# => 2023-04-01T15:30:45+00:00
```

Für mehr Kontrolle oder um Formate zu behandeln, die `parse` möglicherweise nicht direkt versteht, kann man `strptime` (string parse time) verwenden, indem man das Format explizit angibt:

```ruby
# strptime für benutzerdefinierte Formate verwenden
custom_date_string = "01-04-2023"
parsed_date_custom = Date.strptime(custom_date_string, '%d-%m-%Y')
puts parsed_date_custom
# => 2023-04-01
```

### Verwendung von Drittanbieter-Bibliotheken:
Obwohl die integrierten Fähigkeiten von Ruby mächtig sind, zieht man manchmal Drittanbieter-Bibliotheken vor, die zusätzliche Funktionen oder eine einfachere Syntax bieten. Eine beliebte Wahl ist das `Chronic`-Gem für die Verarbeitung natürlicher Sprache:

1. Fügen Sie zunächst Chronic zu Ihrem Gemfile hinzu und führen Sie `bundle install` aus:
```ruby
gem 'chronic'
```

2. Dann verwenden Sie es wie folgt:
```ruby
require 'chronic'

parsed_chronic = Chronic.parse('nächsten Dienstag')
puts parsed_chronic
# Die Ausgabe variiert je nach aktuellem Datum; geht vom Parsen am 2023-04-01 aus
# => 2023-04-04 12:00:00 +0000
```

`Chronic` ist sehr nützlich für Benutzereingaben, da es eine breite Palette von Formaten natürlicher Sprache verstehen kann, was es zu einem leistungsfähigen Werkzeug für Anwendungen macht, die eine flexible Dateneingabe erfordern.
