---
title:                "Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen"
html_title:           "Bash: Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen"
simple_title:         "Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen tarkoittaa tietyn päivämäärän lisäämistä tai vähentämistä tietyllä määrällä päiviä. Tätä tehdään yleensä selkeyden vuoksi tai tarpeen mukaan, kun esimerkiksi lasketaan tulevaa tapahtumaa tai laskutetaan palvelun käyttöaikaa. Ohjelmoijat tekevät tätä yleensä tehokkuuden vuoksi ja välttääkseen virheitä.

## Kuinka tehdä?

```Bash
# Lisää päiviä annettuun päivämäärään
date -d "20 days" # Tulostaa päivämäärän 20 päivän päästä nykyisestä

# Vähennä päiviä annetusta päivämäärästä
date -d "-5 days" # Tulostaa päivämäärän 5 päivää ennen nykyistä

# Lisää päiviä tiettyyn päivämäärään
date -d "2020-01-01 + 1 week" # Tulostaa päivämäärän 1 viikko ensi vuotta myöhemmin

# Muodostaa annetusta päivämäärästä uuden päivämäärän
date -d "2020-06-15 5 days" # Tulostaa päivämäärän 5 päivää jälkeen 15.6.2020
```

## Syvemmälle
Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen on ollut jo pitkään osa ohjelmointia ja löytyy nykyään useista ohjelmointikielistä. Bashissa tämä tapahtuu date-komennolla, mutta esimerkiksi Pythonissa tätä varten on oma datetime-moduuli. Tärkeää on myös huomata, että tarvittaessa päivämäärä pitää muuttaa haluttuun muotoon käyttäen esimerkiksi date-komennon "date +%Y-%m-%d" -parametria.

## Katso myös
- [Date-komennot Bashissa](https://linux.die.net/man/1/date)
- [datetime-moduuli Pythonissa](https://docs.python.org/3/library/datetime.html)