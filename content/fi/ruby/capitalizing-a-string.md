---
title:    "Ruby: Merkkijonon isoiksi kirjoittaminen"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Miksi capitalizing koodiin?

Jos olet uusi Ruby-ohjelmoija, saattaa olla hämmentävää miksi haluaisit capitalisoida merkkijonoa ohjelmassasi. Kuitenkin, capitalisoinnilla on useita käyttötapoja kuten tietojen muokkauksessa ja merkkijonojen muotoilussa.

## Kuinka capitalisointi suoritetaan

Capitalisointi voidaan tehdä käyttämällä `upcase`-metodia. Alla on esimerkki koodista ja siihen liittyvä tulos:

```Ruby
# Alkuperäinen merkkijono
merkkijono = "hyvää päivää"

# Capitalisoitu merkkijono
capitalisoitu_merkkijono = merkkijono.upcase

# Tulostaa: HYVÄÄ PÄIVÄÄ
puts capitalisoitu_merkkijono
```

Yllä olevassa koodissa `upcase`-metodia käytetään capitalisoimaan `merkkijono`-muuttuja ja tallentamaan se uuteen muuttujaan `capitalisoitu_merkkijono`.

## Syvällinen sukellus capitalisoimiseen

On tärkeää huomata, että `upcase`-metodi ei muuta alkuperäistä merkkijonoa, vaan luo uuden capitalisoidun version. Lisäksi `upcase`-metodi toimii vain ASCII-merkkijonoille, eli se ei toimi esimerkiksi ääkkösten tai muiden erikoismerkkien kanssa.

On myös hyvä muistaa, että `upcase`-metodi ei ainoastaan capitalisoi kirjaimia, vaan kaikki merkit muuttuvat suuriksi kirjaimiksi. Esimerkiksi välilyönnit ja välimerkit säilyvät samana.

## Katso myös

- [`upcase`-dokumentaatio](https://ruby-doc.org/core-2.6.3/String.html#method-i-upcase)
- [Ruby-merkkijonon muotoilu](https://www.rubyguides.com/2019/04/ruby-string-methods/)
- [ASCII-merkkijonot](https://en.wikipedia.org/wiki/ASCII)