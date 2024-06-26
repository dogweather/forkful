---
date: 2024-01-20 17:30:54.171443-07:00
description: "How to: (Miten tehd\xE4:) Ennen tietokoneiden aikaa p\xE4iv\xE4m\xE4\
  \xE4r\xE4laskuja tehtiin kalentereiden ja laskimien avulla. Tietokoneet ja skriptausty\xF6\
  kalut, kuten\u2026"
lastmod: '2024-04-05T22:51:11.153051-06:00'
model: gpt-4-1106-preview
summary: "(Miten tehd\xE4:) Ennen tietokoneiden aikaa p\xE4iv\xE4m\xE4\xE4r\xE4laskuja\
  \ tehtiin kalentereiden ja laskimien avulla."
title: "Tulevan tai menneen p\xE4iv\xE4m\xE4\xE4r\xE4n laskeminen"
weight: 26
---

## How to: (Miten tehdä:)
```Fish Shell
# Tuleva päivämäärä 10 päivän kuluttua
set -l future_date (date -d "+10 days" +"%Y-%m-%d")
echo $future_date

# Menneisyyden päivämäärä 10 päivää sitten
set -l past_date (date -d "-10 days" +"%Y-%m-%d")
echo $past_date
```
Näyttää:
```
2023-04-21 # jos tämä on 10 päivää tulevaisuudessa
2023-03-22 # jos tämä on 10 päivää menneisyydessä
```

## Deep Dive (Syväsukellus)
Ennen tietokoneiden aikaa päivämäärälaskuja tehtiin kalentereiden ja laskimien avulla. Tietokoneet ja skriptaustyökalut, kuten `date`, tekevät tämän nopeammin ja tarkemmin. `date` on klassinen UNIX-komennon, joka toimii Fish Shellissä ja muissa shelleissä. Vaihtoehtoisia työkaluja päivämäärälaskentaan ovat esimerkiksi `ntpd`, `chrony`, tai eri ohjelmointikielten kirjastot, kuten Pythonin `datetime`. Fish Shellissa `date`-komennon kanssa käytetyt vaihtoehdot ovat yhtenäiset GNU/date:n kanssa.

## See Also (Katso lisäksi)
- Fish Shell dokumentaatio: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- GNU Coreutils `date`: [https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- Date manipulation in programming languages, Python `datetime`: [https://docs.python.org/3/library/datetime.html](https://docs.python.org/3/library/datetime.html)
