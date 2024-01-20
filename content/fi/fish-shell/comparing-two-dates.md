---
title:                "Kahden päivämäärän vertaaminen"
html_title:           "Bash: Kahden päivämäärän vertaaminen"
simple_title:         "Kahden päivämäärän vertaaminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Vertaamme kahta päivämäärää selvittämään, kumpi niistä sijoittuu aikajanalla ensin. Ohjelmoijat tekevät tämän tärkeiden päivämäärien ja aikataulujen hallitsemiseksi.

## Kuinka tehdään:

>Tässä on Fish Shell -kilpikonnaohjelma, joka vertailee kahta päivämäärää:

```Fish Shell
set -l date1 (date -u -j -f "%Y-%m-%d" "2022-01-01" "+%s")
set -l date2 (date -u -j -f "%Y-%m-%d" "2022-02-01" "+%s")

if [ $date1 -lt $date2 ]
    echo "Date 1 is earlier than Date 2."
else if [ $date1 -eq $date2 ]
    echo "Date 1 is the same as Date 2."
else
    echo "Date 1 is later than Date 2."
end
```

>Tämä ohjelman tuloste on:
```
Date 1 is earlier than Date 2.
```

## Syväsukellus

Fish Shell julkaistiin 12 vuotta sitten, antaen ytimekäsiä ja nopeita tapoja tehdä monimutkaisia toimintoja. Päivämäärien vertaileminen on yksityiskohtaisten prosessien sarja, jossa muunnetaan aika sekunneiksi ja vertaamalla niitä.

Vaihtoehtoisesti voit käyttää 'date2nite' tai 'diff' kutsumaan ajointerfasseja. Vaikka ne voivat olla yhtä tehokkaita, Fish on yksinkertaisempi ja suoraviivaisempi.

Fish Shell päivämäärien vertailussa on tärkeää muistaa, että se linjaa päivämäärät ensin UTC:hen. Sen jälkeen se olettaa, että päivämäärät ovat ilmaistu sekunneissa vuodesta 1970 (UNIX Epoch), ennen kuin ne muunnetaan päivämäärämuotoon.

## Katso myös:

- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Comparison of Shell Scripting Languages](https://www.wikibooks.org/wiki/Bourne_Shell_Scripting)