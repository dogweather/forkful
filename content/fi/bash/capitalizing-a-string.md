---
title:                "Bash: Merkkijonon ensimmäisen kirjaimen suurennus"
simple_title:         "Merkkijonon ensimmäisen kirjaimen suurennus"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

 On monia syitä miksi joku haluaisi muuttaa merkkijonon ensimmäisen kirjaimen isompaan kirjaimeen. Se voi olla osa suurempaa Bash-ohjelmaa, jossa tarvitaan yhtenäistä muotoilua merkkijonon kanssa tai se voi vain olla osa henkilökohtaista tyyliä.

## Miten

 Muuttaaksesi merkkijonon ensimmäinen kirjain isompaan kirjaimeen Bashilla, voit käyttää `tr`-komentoa yhdessä `tr 'a-z' 'A-Z'` -syötteen kanssa. Tämä muuttaa jokaisen kirjaimen isompaan kirjaimeen ja palauttaa uuden merkkijonon. Alla on esimerkki ja siihen liittyvä tulos:

```Bash
$ tr 'a-z' 'A-Z' <<< "tämä on esimerkki"
TÄMÄ ON ESIMERKKI
```

Voit myös käyttää `sed`-komennon `s`-toimintoa korvaamaan ensimmäisen kirjaimen isommalla kirjaimella. Alla on esimerkki ja siihen liittyvä tulos:

```Bash
$ sed 's/^./\U&/' <<< "tämä on esimerkki"
Tämä on esimerkki
```

On myös hyödyllistä ottaa huomioon, että merkkijonot voivat sisältää muuttujia ja voi olla tarpeellista käyttää erilaisia muotoilumahdollisuuksia sen mukaan, mitä tarkalleen halutaan.

## Syvällinen sukellus

Bashin `tr`-komento ottaa vastaan kaksi merkkijonoa, joista ensimmäinen määrittää millä merkeillä on korvattava ja toinen millä. Näin ollen `tr 'a-z' 'A-Z'` tarkoittaa, että kaikki merkit välillä a-z korvataan toisella merkkijonolla A-Z. Tämä toiminto tunnetaan myös nimellä transliteraatio.

`sed`-komennossa ensimmäinen osa `s` määrittää mitä halutaan korvata ja toinen osa määrittää millä halutaan korvata. `\U` muuttaa valitun kirjaimen isommaksi ja `&` tarkoittaa muokattavaa merkkiä.

## Katso myös

- Opas Bashin `tr`-komennosta (englanniksi): https://www.tutorialspoint.com/unix_commands/tr.htm
- Bashin `sed`-komennon dokumentaatio (englanniksi): https://www.gnu.org/software/sed/manual/sed.html