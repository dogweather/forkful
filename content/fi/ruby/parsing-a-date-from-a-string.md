---
title:                "Merkkijonosta päivämäärän jäsentäminen"
date:                  2024-01-20T15:38:09.249421-07:00
html_title:           "Bash: Merkkijonosta päivämäärän jäsentäminen"
simple_title:         "Merkkijonosta päivämäärän jäsentäminen"

category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Päivämäärän jäsentäminen merkkijonosta tarkoittaa päivämäärä-arvojen lukemista ja niiden muuttamista ymmärrettävään muotoon. Ohjelmoijat tekevät tätä ymmärtääkseen ja manipuloidakseen päivämääriä, joita käytetään laajasti kaikenlaisissa sovelluksissa.

## Kuinka:
```Ruby
require 'date'

# Luodaan päivämäärä merkkijonosta
date_string = "24-06-2023"
parsed_date = Date.strptime(date_string, "%d-%m-%Y")
puts parsed_date
# Tulostuu: 2023-06-24

# Tämän päivän päivämäärän saaminen
today = Date.today
puts today
# Tulostuu: [Tämän päivän päivämäärä]

# Jäsennys eri formaatilla
datetime_string = "23. June 2023 14:30"
parsed_datetime = DateTime.strptime(datetime_string, "%d. %B %Y %H:%M")
puts parsed_datetime
# Tulostuu: 2023-06-23T14:30:00+00:00
```

## Syventyminen:
Ruby on tarjonnut päivämäärien käsittelyyn omat luokkansa, kuten `Date` ja `DateTime`, jo varhaisista versioista lähtien. Käyttämällä `strptime` metodia, voimme määritellä merkkijonojen tulkitsemisen tarkasti. Tämänkaltaiset työkalut ovat olennaisia, koska päivämäärien formaatit voivat merkittävästi vaihdella sovelluksesta toiseen ja eri maissa.

`Date`-luokka käyttää Gregoriaanista kalenteria välimuistin kanssa, mikä tekee siitä tehokkaan tavalta käsitellä päivämääriä. Vaihtoehtoiset kirjastot, kuten 'Time' tai 'ActiveSupport' (osa Rails-kehyksestä), tarjoavat lisää joustavuutta ja toimintoja.

Selkeyden puolesta Rubylla klassiset päivä- ja aikakirjastot ovat yksinkertaisia ja suoria - useimmiten aivan riittäviä moniin sovellustarpeisiin.

## Katso Myös:
- Ruby's Date class documentation: [Ruby Doc: Date](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html)
- `DateTime` and `Time` class documentation for more complex needs: [Ruby Doc: DateTime](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/DateTime.html), [Ruby Doc: Time](https://ruby-doc.org/core-3.0.0/Time.html)
- TimeZone handling with ActiveSupport: [API Dock: ActiveSupport TimeWithZone](https://apidock.com/rails/ActiveSupport/TimeWithZone)
