---
title:                "Merkkijonon muuttaminen isoin kirjaimin"
html_title:           "Lua: Merkkijonon muuttaminen isoin kirjaimin"
simple_title:         "Merkkijonon muuttaminen isoin kirjaimin"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Stringien suurennuskeulo on prosessi, jossa kaikki kirjaimet muutetaan tekstijonossa isoiksi kirjaimiksi. Tämä voi olla hyödyllistä esimerkiksi virheiden yhtenäistämiseksi tai yhtenäisen ilmeen saamiseksi.

Ohjelmoijat tekevät tätä pääasiassa ylläpitämään koodin yhtenäisyyttä ja estämään virheitä, jotka johtuvat erilaisten kirjoitusasujen käytöstä. Se myös helpottaa virheiden löytämistä, koska kaksi samanlaista sanaa, kuten "avain" ja "AVAIN", näyttävät nyt samalta ja virheitä on helpompi huomata.

## Miten:
```Lua
s = "stringi, joka halutaan suurennetaan"
print("Alkuperäinen string: " .. s)
print("Suurennettu string: " .. string.upper(s))
```

Tulostus:
```
Alkuperäinen stringi: stringi, joka halutaan suurennetaan
Suurennettu stringi: STRINGI, JOKA HALUTAAN SUURENNETTAVANA
```

## Syvällisempi sukellus:
Suurennuskeulo on yleinen prosessi ohjelmoinnissa, ja monet kielet tarjoavat sisäänrakennetun toiminnon tai kirjaston sen tekemiseen. Joissakin kielissä on myös funktionaalisuutta, jolla voidaan muuttaa vain jonkin osan stringistä suurennukseksi, esimerkiksi vain ensimmäinen kirjain sanassa.

Joissakin tapauksissa on parempi käyttää virkettä suuremmalla kirjaimella, mikä tarkoittaa pikkukirjaimella alkavaa lausetta. Tämä voi olla hyödyllistä iskujen tekemisessä, kun asianmukaista virkettä ei tiedetä etukäteen.

On myös tärkeää huomata, että joissakin kielissä suurennuskeulo ei ole tulosta missään, jos merkistö ei sisällä tekstiä. Tämä voi aiheuttaa ongelmia, jos haluat esimerkiksi käsitellä tyhjiä merkkijonoja.

## Katso myös:
- [Lua String Library](https://www.lua.org/pil/20.2.html)
- [Official Lua Documentation](https://www.lua.org/manual/5.3/manual.html#pdf-string.upper)
- [Stack Overflow discussion on capitalizing a string in Lua](https://stackoverflow.com/questions/26085185/how-to-capitalize-a-string-in-lua)