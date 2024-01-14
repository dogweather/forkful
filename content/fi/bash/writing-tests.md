---
title:    "Bash: Testien kirjoittaminen"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Testien kirjoittaminen on olennainen osa Bash-ohjelmointia, koska se auttaa varmistamaan koodisi toimivuuden ja vähentää virheiden riskiä. Testit voivat myös nopeuttaa koodin kehittämistä ja tehdä siitä helpommin ylläpidettävän.

## Miten

Testien kirjoittaminen Bashissa on helppoa. Ensimmäinen askel on luoda testitiedosto, johon tallennat kaikki testitapaukset. Voit käyttää joko Bashin sisäänrakennettua "test" -komentoa tai suositeltua "bats" -komentoa testien kirjoittamiseen.

```Bash
# Sisällytetään "bats" -komento
source /usr/local/bin/bats

# Testitiedosto
@test "Testin nimi" {
  # Testitapausten koodi
  result="$(your_command)"
  [ "$result" == "expected_output" ]
}

# Käynnistä testit
bats your_test_file.bats
```

Tässä esimerkissä käytämme "bats" -komentoa, joka suorittaa kaikki testit testitiedostosta ja antaa tuloksia. Voit myös käyttää Bashin sisäänrakennettua "test" -komentoa, mutta siinä tapauksessa sinun on huolehdittava itse tulosten näyttämisestä.

## Syväsukellus

Kun kirjoitat testejä Bash-ohjelmillesi, on tärkeää ottaa huomioon muutamia asioita. Ensinnäkin, testien tulisi kattaa mahdollisimman monta eri tilannetta ja antaa tarkkoja tuloksia. Toiseksi, käytä muuttujia testien luomiseen, jotta voit helposti muuttaa testattavia parametrejä. Lisäksi, käytä "set -e" -komentoa varmistaaksesi, että testit keskeytyvät välittömästi jos jokin niistä epäonnistuu.

On myös tärkeää muistaa, että testien kirjoittaminen on jatkuva prosessi. Muokkaa ja lisää testejä aina kun koodia muutetaan, jotta voit varmistaa sen toimivuuden.

## Katso myös

- [Bash-testien kirjoittaminen](https://www.linuxjournal.com/article/3587)
- [Bats-dokumentaatio](https://github.com/bats-core/bats-core)
- [Bash-opas aloittelijoille (suomeksi)](https://panerointi.github.io/bash-opas/)