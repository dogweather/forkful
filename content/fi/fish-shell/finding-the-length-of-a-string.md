---
title:                "Fish Shell: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi sijoittaisit aikaa oppiaksesi kuinka löytää merkkijonon pituus Fish Shellilla? Merkkijonon pituuden laskeminen voi olla hyödyllistä esimerkiksi silloin kun haluat tarkistaa, että käyttäjän syöttämä tieto on halutun mittainen tai kun haluat koodissasi tehdä tietyn toiminnon tietyn pituiselle merkkijonolle. Tämä taito voi tehdä koodistasi tehokkaamman ja luotettavamman.

## Kuinka tehdä

Fish Shellilla merkkijonon pituuden löytäminen on helppoa. Voit käyttää komentoa `expr length "merkkijono"`, joka palauttaa merkkijonon pituuden numerona. Voit myös käyttää sheltterimerkkiä `#` merkkijonon perässä ja saada näin selville sen pituuden. 

```
Fish Shell koodi esimerkki:

set string "Hei maailma!"
echo (string | wc)
```

Tämä tulostaisi `1 13`, mikä tarkoittaa yhtä riviä ja 13 merkkiä, joten merkkijonon pituus on 13.

## Syvemmälle

Vaikka merkkijonon pituuden laskeminen Fish Shellilla voi olla yksinkertaista, on tärkeää muistaa muutama asia. Esimerkiksi tyhjää merkkijonoa ei voida laskea, joten jos käyttäjän syöttämä merkkijono on tyhjä, tulisi koodissasi olla tarkistus toiminto valmiina. Lisäksi tulee huomioida, että jos merkkijono sisältää muita merkkejä kuin pelkkiä kirjaimia, lukuunottamatta välilyöntejä, se vaikuttaa myös merkkijonon pituuteen.

## Katso myös

- [Fish Shellin virallinen dokumentaatio](https://fishshell.com/docs/current/index.html)
- [Fish Shell Tutoriaalit](https://fishshell.com/docs/current/tutorial.html)
- [Github: Fish Shell esimerkkejä](https://github.com/fisheryou/writing-a-plugin-for-fish/releases)

Kiitos lukemisesta ja toivottavasti tästä oli hyötyä!