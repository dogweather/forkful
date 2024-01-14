---
title:                "Bash: Merkkijonon muuttaminen isoin kirjaimin"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi joku haluaisi käyttää Bash-ohjelmointia. Bash-ohjelmointi on yksi suosituimmista ohjelmointityökaluista Linux-käyttöjärjestelmän käyttäjille, sillä se on helppo oppia ja käyttää. Yksi yleinen tehtävä Bash-ohjelmoinnissa on merkkijonojen suur- tai pientekirjoittaminen. Tässä blogikirjoituksessa keskitymme nimenomaan siihen, miten merkkijonon voi muuttaa isoiksi kirjaimiksi.

## Kuinka tehdä se

Bash-ohjelmoinnin avulla merkkijonon suur- tai pientekirjoittaminen on helppo tehtävä. Tämä tapahtuu erityisen komennon avulla, nimeltään "tr". Seuraavassa esimerkissä näet, kuinka voit käyttää tätä komentoa muuttaaksesi merkkijonon "Hello World!" isoiksi kirjaimiksi:

```Bash
echo "Hello World!" | tr '[:lower:]' '[:upper:]'
```

Tämän komennon avulla merkkijono "Hello World!" muutetaan muotoon "HELLO WORLD!". Tämä johtuu siitä, että "tr"-komento muuttaa jokaisen kirjaimen merkkijonossa vastaavaan toiseen. Ensimmäinen joukko määrittelee pienet kirjaimet ja toinen joukko suuret kirjaimet.

Voit myös käyttää "tr"-komentoa muuttamaan merkkijonon toisinpäin, eli suuriksi kirjaimiksi pieniksi kirjaimiksi. Seuraavassa esimerkissä näet, kuinka tämä tehdään:

```Bash
echo "Hello World!" | tr '[:upper:]' '[:lower:]'
```

Esimerkin tulostus olisi "hello world!".

## Syvempää tietoa

Monet Bash-ohjelmoijat saattavat ihmetellä, miksi "tr"-komennon käskyt "[:lower:]" ja "[:upper:]" kirjoitetaan juuri tuolla tavalla. Tämä johtuu siitä, että Bash käyttää POSIX-standardia, joka määrittelee nämä merkinnät merkistöjen luokille. Merkistöjen luokkia käytetään määrittelemään, mitä merkkejä halutaan muuttaa. Esimerkiksi jos haluat muuttaa vain tietyn maan merkkijonon merkkejä, voit käyttää merkistöluokkaa, joka vastaa tuon maan merkkejä.

Toinen hyvä ominaisuus, joka liittyy "tr"-komennon käyttöön, on säännöllisten lausekkeiden käyttö. Säännölliset lausekkeet mahdollistavat monimutkaisempien merkistöjen määrittämisen, tietyille "tr"-komennon suorittamille muutoksille.

## Katso myös

Tässä blogikirjoituksessa kävimme läpi yhden tavan muuttaa merkkijonon isoiksi tai pieniksi kirjaimiksi Bash-ohjelmoinnilla. On kuitenkin monia muita tapoja tehdä tämä sama tehtävä. Suosittelemme tutustumaan seuraaviin linkkeihin, joissa voit löytää lisätietoa ja muita vaihtoehtoisia tapoja käsitellä merkkijonoja Bashilla.

- Täydellinen Bash-ohjelmointiopas (englanniksi): https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html 
- Säännölliset lausekkeet Bashissa (englanniksi): https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Regular_Expressions 
- Bash-kirjasto, j