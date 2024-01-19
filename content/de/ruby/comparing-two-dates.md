---
title:                "Vergleich von zwei Daten"
html_title:           "C#: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

---
title: Das Vergleichen von zwei Daten in Ruby
---

# Was & Warum?

In der Programmierung vergleichen wir häufig zwei Daten, um zu ermitteln, ob ein Datum vor, nach oder gleich einem anderen ist. Dies ist nützlich, um Zeitabschnitte zu bestimmen, Veranstaltungen einzuplanen oder Abläufe kontrollieren.

# Wie man das macht:

Ruby macht den Vergleich von Daten sehr einfach. Die folgenden Beispiele zeigen, wie man es macht.

```ruby
# Erstellen Sie zwei Zeitobjekte
datum1 = Time.new(2020, 10, 31)
datum2 = Time.new(2021, 10, 31)

# Überprüfen, ob datum1 vor datum2 liegt
if datum1 < datum2
  puts "datum1 ist früher!"
else
  puts "datum1 ist später oder gleich!"
end
```
Mit diesem Code wird "datum1 ist früher!" ausgeben, da der 31. Oktober 2020 vor dem 31. Oktober 2021 liegt.

### Tiefer Eintauchen

In Ruby ist die Klasse 'Time' für die Darstellung von Zeitpunkten zuständig. Der historische Hintergrund ist interessant, da das Vergleichen von Daten in der Informatik grundlegend ist und im Kern der Betriebssysteme und Datenbanksysteme steht.

Es gibt alternative Methoden zur Datumsvergleich, wie die Verwendung der `Date`-Klasse oder umfangreichere Bibliotheken wie `ActiveSupport::TimeWithZone`. 

Zum Implementierungsdetail: die Vergleichsoperatoren `<, >, ==` sind in der `Time`-Klasse in Ruby überschrieben, was den direkten Vergleich dieser Objekte ermöglicht. 

#### Siehe auch:

 Um mehr über die Verwendung der `Time`-Klasse in Ruby zu erfahren, finden Sie Informationen in der offiziellen Dokumentation [hier](https://ruby-doc.org/core-2.7.0/Time.html).

Für detailliertere Informationen über `ActiveSupport::TimeWithZone`, sehen Sie sich [diesen Link](https://api.rubyonrails.org/classes/ActiveSupport/TimeWithZone.html) an.

---