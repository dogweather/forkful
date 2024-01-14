---
title:                "Bash: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi

Joskus Bash-ohjelmoinnissa on tarpeen verrata kahta päivämäärää toisiinsa. Tämä voi johtua esimerkiksi tarpeesta tarkistaa, kumpi päivämäärä on uudempi tai selvittää, kuinka monta päivää on kulunut kahden päivämäärän välillä. Tässä blogikirjoituksessa käymme läpi, miten vertaaminen kahden päivämäärän välillä onnistuu Bashilla ja mitä erityistä tulee huomioida.

## Miten

Vertaaminen kahden päivämäärän välillä onnistuu Bashilla käyttämällä muuttujia ja käyttämällä niille tarkoitettuja vertailuoperaattoreita. Seuraavassa esimerkissä luomme kaksi muuttujaa, jotka sisältävät päivämääriä ja vertaamme niitä keskenään.

```Bash
# Luodaan muuttujat
paivamaara1="2021-01-01"
paivamaara2="2020-12-31"

# Vertaillaan muuttujia
if [[ "$paivamaara1" > "$paivamaara2" ]]
then
    echo "Ensimmäinen päivämäärä on uudempi."
elif [[ "$paivamaara1" < "$paivamaara2" ]]
then
    echo "Toinen päivämäärä on uudempi."
else
    echo "Päivämäärät ovat samat."
fi

```

Tämän esimerkin avulla voimme selvittää, kumpi päivämäärä on uudempi ja tulostaa siitä viestin. Käymme nyt läpi muutamia esimerkkejä, miten voimme hyödyntää tätä vertailutoimintoa.

### Tarkistaminen uudemmasta päivämäärästä

Jos esimerkiksi haluamme tarkistaa, onko ensimmäinen päivämäärä uudempi, voimme käyttää seuraavaa ehtolauseketta:

```Bash
if [[ "$paivamaara1" > "$paivamaara2" ]]
then
    # Ensimmäinen päivämäärä on ehdottomasti uudempi
fi
```

Tässä tapauksessa ehtolauseke toteutuu vain, jos ensimmäinen päivämäärä on jälkeen toisen päivämäärän.

### Tarkistaminen vanhemmasta päivämäärästä

Jos haluamme puolestaan tarkistaa, onko ensimmäinen päivämäärä vanhempi, voimme käyttää seuraavaa ehtolauseketta:

```Bash
if [[ "$paivamaara1" < "$paivamaara2" ]]
then
    # Ensimmäinen päivämäärä on ehdottomasti vanhempi
fi
```

### Päivien määrän laskeminen

Voimme myös laskea päivien määrän kahden päivämäärän välillä käyttämällä unix-komentorivillä olevaa `date`-työkalua ja Bashin `date`-funktiota. Esimerkiksi jos haluamme tietää, kuinka monta päivää on kulunut ensimmäisen ja toisen päivämäärän välillä, voimme käyttää seuraavaa lähestymistapaa:

```Bash
paivien_maara=$(( (`date -d "$paivamaara1" +%s` - `date -d "$paivamaara2" +%s`) / 86400 ))
```

Tässä lasketaan ensin päivämäärien välisen sekuntimäärän erotus ja jaetaan se päivän sekuntimäärällä (86 400 sekuntia). Tämän jälkeen päivien määrä tulee talteen muuttujaan `paivien_ma