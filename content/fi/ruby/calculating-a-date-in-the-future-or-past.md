---
title:                "Tulevan tai menneen päivämäärän laskeminen"
html_title:           "Ruby: Tulevan tai menneen päivämäärän laskeminen"
simple_title:         "Tulevan tai menneen päivämäärän laskeminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Tulevaisuuden tai menneisyyden päivämäärän laskeminen tarkoittaa odotetun päivämäärän arvioimista lisäämällä tai vähentämällä päiviä, viikkoja, kuukausia tai vuosia nykyhetkestä. Ohjelmoijat tekevät tämän usein aikataulutettuja tehtäviä tai tapahtumia varten, kuten esimerkiksi määräaikojen hallintaan tai eräpäivien seuraamiseen.

## Miten se tehdään:

Rubyssa voit laskea tulevan tai menneisyyden päivämäärän 'Date' -luokan avulla. Tässä yksinkertainen esimerkki siitä, miten se tehdään:

```Ruby
require 'date'

# Nykyhetki
nyt = Date.today

# Laske 5 päivää tulevaisuuteen
tulevaisuus = nyt + 5
puts "Viiden päivän päästä on #{tulevaisuus}"

# Laske 7 päivää menneisyyteen
menneisyys = nyt - 7
puts "VIIkon sitten oli #{menneisyys}"
```
Tämä koodinpätkä antaa sinulle tulosteena kaksi päivämäärää: yhden viiden päivän päästä tulevaisuudessa ja toisen viikon päästä menneisyydessä.

## Sukellus syvemmälle

Ruby ratkaisee tulevaisuuden ja menneisyyden päivämäärän laskemisen 'Date' -luokan kautta. Vaikka tämä on yksinkertainen ja suoraviivainen ratkaisu, sillä on mielenkiintoinen historiallinen konteksti. Ajatukset ajan laskemisesta juontavat juurensa varhaiseen tietojenkäsittelyyn, jolloin järjestelmien kapasiteetit ja laskentakyky olivat rajalliset.

Ruby ei ole ainoa kieli, joka tarjoaa tällaisia ominaisuuksia. Useimmat modernit ohjelmointikielet, kuten Python, JavaScript ja C#, tarjoavat omat menetelmänsä tämän ongelman ratkaisemiseen. Ruby on kuitenkin valtavan yksinkertaisuutensa ansiosta erityisen suosittu vaihtoehto.

Lisäksi on syytä huomata, että vaikka 'Date' -luokka on yksinkertainen ja helppo tapa manipuloida päivämääriä, sitä ei tulisi käyttää ilman huolellista harkintaa. Käyttämällä tätä luokkaa sallit Ruby-ohjelman suorittaa korkean tason laskutoimituksia päivämäärien kanssa, minkä takia saatat kohdata aikaan liittyviä kompleksisuuksia ja hankaluuksia, kuten karkausvuodet tai aikavyöhykkeet.

## Katso myös:

- [Date-luokan dokumentaatio](https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html)
- [Ajankäsittelyn neuvoja Rubyn kanssa](https://www.justinweiss.com/articles/3-ways-to-monkey-patch-without-making-a-mess/)
- [Pythonin datetime-moduuli](https://docs.python.org/3/library/datetime.html)
- [JavaScriptin Date-objekti](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [C# DateTime-luokka](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)