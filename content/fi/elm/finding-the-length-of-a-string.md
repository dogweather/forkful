---
title:    "Elm: Saippuakivikauppias"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi haluat löytää merkkijonon pituuden. Ehkä haluat varmistaa, että syöttämäsi tiedot ovat oikean mittaisia tai ehkä haluat vain käyttää tätä taitoa omassa ohjelmointiprojektissasi.

## Kuinka tehdä se

Elm tarjoaa yksinkertaisen ja helpon tavan löytää merkkijonon pituus. Voit käyttää sisäänrakennettua `String.length` -funktiota, joka ottaa parametrinaan merkkijonon ja palauttaa sen pituuden kokonaislukuna.

```Elm
let sana = "Hei maailma!"
String.length sana
```

Tämä koodi tulostaisi seuraavan:

```
12
```

## Syvällisempi tieto

Vaikka `String.length` -funktio on helppo tapa löytää merkkijonon pituus, on hyvä ymmärtää miten se toimii taustalla. Merkkijonon pituus lasketaan yleensä käymällä läpi jokainen merkki ja lisäämällä yksi laskuriin jokaista löydettyä merkkiä kohden. Elm:ssä tämä työ tehdään automaattisesti `String.length` -funktion avulla.

## Katso myös

- [Elm String Documentation](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm Language Guide](https://guide.elm-lang.org/)