---
title:    "C++: Merkkijonon pituuden löytäminen."
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit selvittää merkkijonon pituuden? On monia tilanteita, joissa tarvitset tätä tietoa ohjelmoinnissa. Esimerkiksi jos haluat leikata merkkijonosta tietyn määrän merkkejä pois, sinun täytyy tietää sen pituus ennen kuin voit suorittaa leikkaustoiminnon. Toinen yleinen syy on käsitellä käyttäjän antamia syötteitä, joiden pituus voi vaihdella ja sinun täytyy olla varma siitä, että syöte ei ylitä ennalta määrättyä pituutta.

## Kuinka

Onneksi C++:ssa on valmiiksi sisäänrakennettu funktio, joka palauttaa merkkijonon pituuden, nimeltään `length()`. Voit käyttää sitä helposti seuraavasti:

```C++
// Luodaan merkkijono "Hei maailma"
std::string s = "Hei maailma";

// Tulostetaan merkkijonon pituus
std::cout << s.length() << std::endl;

// Output: 11
```

Tämä funktio palauttaa kokonaisluvun, joka vastaa merkkijonon pituutta. Huomaa, että tyhjä merkkijono palauttaa pituuden 0.

## Syventävä tarkastelu

Jos haluat tietää, kuinka `length()` toimii taustalla, se perustuu merkkijonon sisäisen muistin käsittelyyn. Jokaisella merkkijonolla on "piilossa" tieto sen pituudesta. Tämä tieto päivittyy automaattisesti aina kun muokkaat merkkijonoa, joten sinun ei tarvitse huolehtia sen päivittämisestä manuaalisesti. Tämä pituuden tieto tallennetaan `string` luokan yhteydessä olevaan luokkaan, joten voit aina käyttää `length()` funktiota riippumatta siitä, mitä merkkijonon metodia käytät.

## Katso myös

- [`string.length()`-dokumentaatio](http://www.cplusplus.com/reference/string/string/length/)
- [`string`-luokan dokumentaatio](http://www.cplusplus.com/reference/string/string/)