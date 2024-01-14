---
title:                "Javascript: Tekstin etsiminen ja korvaaminen"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Miksi

Monet ohjelmointiprojektit vaativat tekstin muokkausta, ja aina silloin tällöin täytyy vaihtaa yksittäisiä sanoja tai lauseita koodissa. Ei ehkä ole järkevää tehdä tätä manuaalisesti, varsinkin jos tekstiä on paljon. Siinä tulee tarpeelliseksi tekstinhakutoiminto.

# Miten

Tekstihakutoiminnon käyttö on melko yksinkertaista. Ensinnäkin, luo muuttuja, johon tallennetaan haluttu teksti. Esimerkiksi:

```Javascript
let teksti = "Tämä on esimerkki tekstistä."
```

Sitten voit käyttää `.replace()`-metodia, joka hyväksyy kaksi parametria: etsittävän tekstin ja korvaavan tekstin. Esimerkiksi, jos haluat vaihtaa sanan "esimerkki" sanaan "esimerkki tekstistä", se olisi:

```Javascript
let korjattuTeksti = teksti.replace("esimerkki", "esimerkki tekstistä");
```

Huomaa, että `.replace()`-metodi antaa takaisin uuden merkkijonon, joten tallenna se uuteen muuttujaan. Jos haluat korvata kaikki esiintymät tekstin sisällä, voit käyttää lausetta "g" parametrina:

```Javascript
let korjattuTeksti = teksti.replace(/esimerkki/g, "esimerkki tekstistä");
```

Nyt korjattuTeksti-muuttuja sisältää alkuperäisen tekstin, jossa kaikki esiintymät sanasta "esimerkki" on korvattu sanalla "esimerkki tekstistä". Voit myös antaa toisen muuttujan `.replace()`-metodin toisena parametrina, jotta voit käyttää sitä tekstiin, joka halutaan korvata. Esimerkiksi:

```Javascript
let korvaaja = "muokattu";
let korjattuTeksti = teksti.replace(/esimerkki/g, korvaaja);
```

Tämä korvaa kaikki esiintymät sanasta "esimerkki" muuttujan "korvaaja" sisällöllä. Lopputulos olisi "Tämä on muokattu tekstistä."

# Syvempi sukellus

`.replace()`-metodi hyväksyy myös toisen parametrin, joka voi olla funktio. Tämä funktio käsittelee jokaisen tekstin esiintymän erikseen ja palauttaa uuden tekstin, joka korvataan alkuperäisessä tekstittringissä. Esimerkiksi:

```Javascript
let korjattuTeksti = teksti.replace(/esimerkki/g, (osoittama) => {
  return osoittama.toUpperCase(); // muuttaa korvatun tekstin isoihin kirjaimiin
});
```

Tämän avulla voit suorittaa enemmän monimutkaisia muutoksia tekstin sisällä, joita et voi tehdä yksinkertaisesti antamalla korvaavan merkkijonon parametrina.

# Katso myös

- [MDN web docs: .replace()](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [W3Schools: JavaScript String replace() Method](https://www.w3schools.com/jsref/jsref_replace.asp)
- [Tutorialspoint: JavaScript String replace() Method](https://www.tutorialspoint.com/javascript/string_replace.htm)