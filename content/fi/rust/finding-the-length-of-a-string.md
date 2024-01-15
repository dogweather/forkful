---
title:                "Merkkijonon pituuden löytäminen"
html_title:           "Rust: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi haluat löytää merkkijonon pituuden ohjelmointitehtävässäsi. Ehkä haluat varmistaa, että merkkijono on tietyn pituinen ennen sen lähettämistä eteenpäin, tai ehkä haluat ohjelman suorittavan tietyn toiminnon vain, jos merkkijonon pituus täyttää tietyt vaatimukset. Riippumatta syistä, Rustilla on helppo tapa löytää merkkijonon pituus ja tässä artikkelissa opit, miten se tehdään.

## Kuinka

Jos haluat löytää merkkijonon pituuden Rustilla, sinun tarvitsee vain käyttää `len()`-metodia. Tämä metodi on käytettävissä kaikilla merkkijonoilla ja se palauttaa merkkijonon pituuden kokonaislukuna. Alla on esimerkki koodi, jossa laskemme merkkijonon "Hei maailma!" pituuden ja tulostamme sen näytölle:

```Rust
let merkkijono = "Hei maailma!";
let pituus = merkkijono.len();

println!("Merkkijonon pituus on {}", pituus);
```

Kun suoritat tämän koodin, saat tulosteen "Merkkijonon pituus on 12". Huomaa, että välilyönnit ja erikoismerkit lasketaan myös merkkijonon pituuteen.

## Syvempi sukellus

On tärkeää ymmärtää, että merkkijonon pituus ei ole sama asia kuin merkkijonon kapasiteetti. Kapasiteetilla tarkoitetaan tilaa, joka on varattu merkkijonolle mutta jota ei välttämättä käytetä. Voit tarkistaa merkkijonon kapasiteetin käyttämällä `capacity()`-metodia. Lisäksi, jos haluat muuttaa merkkijonon kapasiteettia, voit käyttää `reserve()`-metodia. Tämä on hyödyllistä esimerkiksi jos tiedät, että merkkijono kasvaa tulevaisuudessa ja haluat varata sille lisää tilaa etukäteen.

Lisäksi, jos haluat löytää merkkijonon pituuden lisäksi myös sen tavujen (bytes) määrän, voit käyttää `as_bytes()`-metodia. Tämä palauttaa tavuja sisältävän taulukon, josta voit laskea tavujen määrän.

## Katso myös

- [Rustin dokumentaatio merkkijonojen käytöstä](https://doc.rust-lang.org/std/string/struct.String.html)
- [MDN Web Docs - Merkkijonot](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)