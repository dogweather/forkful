---
title:                "Python: Merkkijonon kirjoittaminen isoksi"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi muuttaa merkkijonon ensimmäisen kirjaimen isoksi kirjaimeksi?

Isolla alkukirjaimella kirjoitettu merkkijono voi näyttää tyylikkäämmältä ja helpommin luettavalta. Tämä voi olla erityisen tärkeää esimerkiksi ohjelmointitehtävissä, joissa selkeä koodi on avainasemassa.

## Miten

Voit muuttaa merkkijonon ensimmäisen kirjaimen isoksi kirjaimeksi käyttämällä Pythonin sisäistä capitalize()-metodia. Katso alla oleva esimerkki:

```Python
string = "tämä on esimerkki"
print(string.capitalize())
```

Tulosteena saat "Tämä on esimerkki". Huomaa, että capitalize()-metodi ei muuta muita kirjaimia, vaan pelkästään ensimmäisen kirjaimen isoksi.

Voit myös käyttää capitalize()-metodia yhdessä strip()-metodin kanssa, jolloin koodi poistaa ensin ylimääräiset välilyönnit merkkijonosta ennen ensimmäisen kirjaimen muuttamista isoksi.

## Syväsyvennys

Merkkijonon muuttaminen isolla alkukirjaimella on vain yksi esimerkki siitä, miten voit manipuloida merkkijonoja Pythonilla. Voit muun muassa muuttaa koko merkkijonon isoiksi tai pieniksi kirjaimiksi, poistaa tiettyjä merkkejä tai lisätä uusia merkkejä.

Lisäksi kannattaa muistaa, että Python käyttää Unicode-standardia, joten merkkijonoihin voi lisätä myös muita kuin englannin kielen merkkejä.

Lopuksi, kannattaa kokeilla myös muita Pythonin sisäisiä metodeja ja funktioita merkkijonojen manipulointiin. Kirjoita merkkijonoja, muokkaa niitä ja katso mitä tapahtuu!

## Katso myös

- [Pythonin merkkijonojen käsittely (Ohjelmointiopas.fi)](https://www.ohjelmointiopas.fi/python/kirjastoja/merkkijonot/)
- [Pythonin virallinen dokumentaatio merkkijonojen käsittelystä](https://docs.python.org/3.8/library/stdtypes.html#text-sequence-type-str)
- [Pythonin merkkijonojen käsittely (Tutkimus Apaja)](https://www.tutkimusapaja.fi/pythonin-merkkijonojen-kasittely/)