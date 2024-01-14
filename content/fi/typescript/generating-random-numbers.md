---
title:    "TypeScript: Satunnaisten numeroiden luominen"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi
Ohjelmoinnin maailmassa satunnaislukujen generointia tarvitaan monessa eri tilanteessa. Se voi olla esimerkiksi pelien hauskaa lisämaustetta, lottokoneen arvontojen taustalla tai vaikka salasanoiden luomisessa. Satunnaislukujen generointi onkin hyödyllinen taito jokaiselle ohjelmoijalle.

## Kuinka
Satunnaislukujen generointi TypeScriptillä onnistuu helposti Math-luokan avulla. Käytämme tässä esimerkissä Math.random() -metodia, joka palauttaa desimaalilukuna väliltä 0.0 ja 1.0. Seuraavassa koodiesimerkissä näytämme, kuinka generoida viisi satunnaista kokonaislukua väliltä 1-10:

```TypeScript
for (let i = 0; i < 5; i++) {
  let randomNum = Math.round(Math.random() * 9 + 1);
  console.log(randomNum);
}
```
Tulostus:
```
7
4
10
2
9
```
Voimme myös muuttaa generoitavan numeron alku- ja loppurajaa haluamallamme tavalla esimerkiksi käyttämällä Math.ceil() ja Math.floor() funktioita. Seuraavassa esimerkissä generoidaan viisi satunnaista parillista kokonaislukua väliltä 2-10:

```TypeScript
for (let i = 0; i < 5; i++) {
  let randomNum = Math.floor(Math.random() * 5 + 2) * 2;
  console.log(randomNum);
}
```

Tulostus:
```
10
4
8
6
10
```

## Syvemmälle
Satunnaislukujen generointi perustuu pseudosatunnaislukuteoriaan, jossa käytetään matemaattisia kaavoja luomaan numerosarjoja, jotka vaikuttavat sattumanvaraisilta. Nämä kaavat tarvitsevat lähtökohdakseen ns. siemenluvun, joka on yleensä järjestysluku tai ajanhetki millisekunneissa. Math.random() -metodi käyttää tätä siemenlukua taustalla ja luo sen perusteella numerosarjan, joka näyttää sattumanvaraiselta.

Satunnaislukujen generoinnissa on myös tärkeää ottaa huomioon seedin arvo eli siemenluku, jota käytetään luomaan numerot. Jos seedin arvo on aina sama, myös generoidut numerot toistuvat samassa järjestyksessä. Tämä voi olla huono asia esimerkiksi salasanoja generoidessa, jossa tarvitaan todellista sattumanvaraisuutta. Seedin arvo kannattaa siis vaihtaa aina kun halutaan uusi satunnainen numerosarja.

## Katso myös
- [Random number generation in TypeScript](https://medium.com/@thisisjason/random-number-generation-in-typescript-b09257ac769d)
- [Math.random() documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Pseudorandom number generator](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)