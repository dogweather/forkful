---
title:    "Fish Shell: Testien kirjoittaminen"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Miksi

Kirjoita testejä! Testit ovat tärkeä osa ohjelmointiprosessia, ja ne auttavat varmistamaan, että koodi toimii suunnitellulla tavalla. Lisäksi testien avulla voit tunnistaa ja korjata mahdolliset virheet ennen kuin ne aiheuttavat ongelmia käytössä.

## Miten

Jos käytät Fish Shellia ohjelmointikieleksi, voit helposti luoda testejä käyttämällä `test`-komentoa. Esimerkiksi:

```Fish Shell
function greet
  echo "Tervetuloa, $argv[1]!"
end

test "greet should greet the given person" -d
status-is 0
greet "Jussi"
stdout-is "Tervetuloa, Jussi!"
```

Tässä koodiesimerkissä määritellään `greet`-funktio, joka tulostaa annetun henkilön nimen tervehdyksenä. Testissä tarkistetaan, että funktio toimii odotetulla tavalla ja tulostaa oikean viestin. Voit myös käyttää muita testin asettamiseen liittyviä komentoja, kuten `status-is` ja `stdout-is`, varmistaaksesi, että testi tarkistaa oikeat asiat.

## Syvällinen sukellus

Testien kirjoittamisella on monia etuja, kuten liiketoiminnan esto ja koodin laadun varmistaminen. Lisäksi testien käyttöönotto auttaa sinua keskittymään yksittäisiin toiminnallisuuksiin kerrallaan, mikä tekee ohjelmoinnista hallittavampaa ja vähentää virheiden mahdollisuutta.

On myös tärkeää muistaa, että testien kirjoittaminen ei ole vain yksittäinen tehtävä, vaan jatkuva prosessi. Testien päivitys ja ylläpito auttavat sinua pitämään koodisi toimivana ja virheettömänä.

## Katso myös

- [Fish Shellin viralliset dokumentaatiot](https://fishshell.com/docs/current/index.html)
- [Blogikirjoitus unit testien kirjoittamisesta Fish Shellilla](https://lobste.rs/s/705xmm/unit_testing_fish_shell)
- [Github-projekti Fish Shellin `test`-komennon kehittämisestä](https://github.com/fish-shell/fish-shell/pull/5112)