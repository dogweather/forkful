---
title:                "Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
html_title:           "Ruby: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
simple_title:         "Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen on tärkeä osa ohjelmointia, koska se auttaa meitä hallitsemaan aikaa ja pitämään sovelluksiamme päivitettyinä. Se on myös hyödyllistä esimerkiksi tapahtuma- ja varaussovelluksissa, joissa tarvitaan tietoa tulevista päivämääristä.

## Miten:

Laskeminen tapahtuu Ruby-ohjelmointikielellä helposti käyttämällä `Date`-luokan metodeja. Esimerkiksi, jos haluat laskea päivämäärää tulevaisuuteen kolmen päivän verran, voit käyttää seuraavaa koodia:

```ruby
require 'date'

pvm = Date.today + 3
puts pvm
```

Tämä koodi tulostaisi kolmen päivän päästä olevan päivän päivämäärän. Vastaavasti voit käyttää `Date`-luokan metodeja myös päivämäärien vähentämiseen menneisyydessä.

## Syvempään sukellus:

Päivämäärän laskemisen tarve on ollut olemassa jo aikojen alusta saakka, sillä ihmiset ovat halunneet hallita ja tietää aikaa sekä tapahtumia tulevaisuudessa. Myös muissa ohjelmointikielissä on saatavilla vastaavia työkaluja päivämäärien laskemiseen.

## Katso myös:

Voit lukea lisää `Date`-luokasta ja sen metodeista Ruby-dokumentaatiosta: https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html