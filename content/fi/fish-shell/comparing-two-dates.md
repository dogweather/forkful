---
title:    "Fish Shell: Kahden päivämäärän vertailu"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit verrata kahta päivämäärää Fish Shell -ohjelmoinnissa? On monia syitä, jotka voivat johtaa tähän tarpeeseen. Saatat esimerkiksi haluta tarkistaa, onko tietty tapahtuma tapahtunut ennen vai jälkeen toisen tapahtuman, tai laskea ajanjaksojen välinen ero. Fish Shellin avulla voit helposti vertailla päivämääriä ja suorittaa haluamasi toiminnot.

## Miten

Fish Shell käyttää omaa sisäänrakennettua `date` komentoa päivämäärien käsittelyyn. Voit verrata kahta päivämäärää käyttämällä `date -u` komentoa ja antamalla päivämäärät parametreiksi. Seuraavassa esimerkissä verrataan päivämäärää 25. helmikuuta 2020 ja nykyistä päivämäärää:

```Fish Shell
date -u +%Y-%m-%d 25.02.2020 | read first
date -u +%Y-%m-%d | read second
```

Tämän jälkeen voit käyttää Fish Shellin sisäistä `test` komentoa ja `if` ehtolausetta vertaillaksesi kahta päivämäärää:

```Fish Shell
if test $first -lt $second
    echo "Ensimmäinen päivämäärä on ennen toista päivämäärää."
else if test $first -gt $second
    echo "Ensimmäinen päivämäärä on jälkeen toista päivämäärää."
else
    echo "Päivämäärät ovat samoja."
end
```

Tulisi tulostaa `Ensimmäinen päivämäärä on ennen toista päivämäärää.`

## Syvempää sukellusta

Fish Shellin `date -u` komento tukee erilaisia vaihtoehtoja, joiden avulla voit muokata päivämäärien esitystapaa. Voit esimerkiksi käyttää `-f` parametria ja annettua muotoa päivämäärien tarkempaan vertailuun. Voit myös käyttää `-r` parametria ja antaa päivämäärälukuarvoja, mikä helpottaa päivämäärien lisäämistä tai vähentämistä.

See Also (Katso myös):
- [Fish Shellin virallinen dokumentaatio päivämääränmuokkauksesta](https://fishshell.com/docs/current/cmds/date.html)
- [Vertaile päivämääriä Fish Shellin avulla](https://www.brianchildress.co/post/comparing-dates-fish)