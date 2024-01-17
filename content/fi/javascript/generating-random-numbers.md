---
title:                "Sattumanvaraisten lukujen luominen"
html_title:           "Javascript: Sattumanvaraisten lukujen luominen"
simple_title:         "Sattumanvaraisten lukujen luominen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?

Satunnaislukujen generointi on tapa tuottaa numeroita arpomalla niitä tietyn alueen sisällä. Tämä on hyödyllistä monissa ohjelmoinnin konteksteissa, kuten pelien kehityksessä tai salauksen avaimien generoimisessa, koska tällä tavalla voidaan luoda lukuja ilman tiettyä järjestystä tai ennustettavuutta.

# Miten:

Esimerkki 1:

```
// Generoi satunnainen luku väliltä 1-10
let randomNum = Math.floor(Math.random() * 10) + 1;

console.log(randomNum);
// Output: 6
```

Esimerkki 2:

```
// Generoi satunnainen värin RGB-arvot
let red = Math.floor(Math.random() * 256);
let green = Math.floor(Math.random() * 256);
let blue = Math.floor(Math.random() * 256);

console.log(`RGB(${red}, ${green}, ${blue})`);
// Output: RGB(34, 189, 156)
```

# Syvemmälle:

Historiallinen konteksti:

Satunnaislukujen generointi on ollut osa ohjelmointia jo pitkään, ja siihen liittyviä algoritmeja on kehitetty vuosien saatossa. Alkuaikoina käytettiin esimerkiksi itse luotuja kaavoja, mutta nykyään yleisesti käytössä olevat tarkemmat ja laajemmat algoritmit ovat vähentäneet mahdollisuuksia ennustettavuuteen tai toistettavuuteen.

Vaihtoehtoja:

Lisäksi Math.random() -funktion lisäksi on olemassa muita tapoja generoida satunnaislukuja, kuten esimerkiksi käyttämällä erilaisia matemaattisia kaavoja tai kirjastoja, kuten "random-js" tai "chance".

Toteutus:

Satunnaislukujen generointi on toteutettu monissa ohjelmointikielissä sisäisillä funktioilla tai algoritmeilla. Hyvin usein käytetään "pseudo-satunnaislukuja", jotka eivät ole täysin satunnaisia, vaan perustuvat tiettyyn lähtöarvoon. Tällöin samaa lähtöarvoa käytettäessä saadaan aina sama lopputulos.

# Katso myös:

- https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/Math/random
- https://www.random.org/randomness/
- https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/Math/random#Description