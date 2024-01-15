---
title:                "Umwandeln eines Datums in einen String"
html_title:           "Ruby: Umwandeln eines Datums in einen String"
simple_title:         "Umwandeln eines Datums in einen String"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren von Datumsangaben in Strings ist ein häufiger Vorgang bei der Erstellung von Programmen. Oftmals müssen Nutzer von Programmen oder Webseiten das Datum in einem bestimmten Format angezeigt bekommen, daher ist es wichtig zu wissen, wie man eine Datumsangabe in einen String umwandelt.

## Wie geht's

```ruby
# Verwende die Methode `strftime` um das Datum in einen String umzuwandeln
date = Time.now
date_string = date.strftime("%d.%m.%Y")

# Die erste Variable `date` enthält das aktuelle Datum und die zweite Variable `date_string` enthält den String mit dem gewünschten Format
puts date # => 2021-01-01 14:30:00 +0100
puts date_string # => 01.01.2021
```

Man kann auch spezifische Teile des Datums auswählen und in den String einfügen:

```ruby
date_string = date.strftime("Es ist %H:%M Uhr am %d. %B %Y.")
puts date_string # => Es ist 14:30 Uhr am 01. Januar 2021.
```

Es gibt viele verschiedene Formatierungsmöglichkeiten, um das Datum in einen String umzuwandeln. Hier sind einige Beispiele:

<table>
  <tr>
    <th>Zeichen</th>
    <th>Beschreibung</th>
    <th>Beispiel</th>
  </tr>
  <tr>
    <td>%d</td>
    <td>Tag des Monats (01 bis 31)</td>
    <td>01</td>
  </tr>
  <tr>
    <td>%m</td>
    <td>Monat (01 bis 12)</td>
    <td>01</td>
  </tr>
  <tr>
    <td>%B</td>
    <td>Monatsname (Januar bis Dezember)</td>
    <td>Januar</td>
  </tr>
  <tr>
    <td>%Y</td>
    <td>Jahr (vierstellig)</td>
    <td>2021</td>
  </tr>
  <tr>
    <td>%H</td>
    <td>Stunde (00 bis 23)</td>
    <td>14</td>
  </tr>
  <tr>
    <td>%M</td>
    <td>Minute (00 bis 59)</td>
    <td>30</td>
  </tr>
  <tr>
    <td>%S</td>
    <td>Sekunde (00 bis 59)</td>
    <td>00</td>
  </tr>
</table>

Weitere Formatierungsmöglichkeiten findet man in der offiziellen Ruby-Dokumentation unter [Time#strftime](https://ruby-doc.org/core-3.0.1/Time.html#method-i-strftime).

## Tiefenschärfe

Wenn man `strftime` verwendet, kann man auch bestimmte Attribute aus dem Datum auswählen und in den String einfügen. Eine vollständige Liste aller möglichen Attribute findet man ebenfalls in der offiziellen Dokumentation unter [Time#strftime](https://ruby-doc.org/core-3.0.1/Time.html#method-i-strftime).

Es ist auch möglich, das Datum in eine andere Sprache zu konvertieren, indem man das Sprachkürzel nach dem `%` einfügt, z.B. `%d.%m.%y %H:%M Uhr` für das deutsche Format.

## Siehe auch

- [DateTime#strftime](https://ruby-doc.org/stdlib-3.0.1/libdoc/date/rdoc/DateTime.html#method-i-strftime)
- [String interpolation in Ruby](https://www.rubyguides.com/2019/02/ruby-string-interpolation/)