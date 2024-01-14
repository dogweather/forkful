---
title:    "Fish Shell: Muunna merkkijono pieniksi kirjaimiksi."
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Miksi

Jokaisessa ohjelmointikielessä on tärkeää, että tiedämme, kuinka muutamme merkkijonon isoiksi tai pieniksi kirjaimiksi. Tämä on erityisen hyödyllistä, kun haluamme vertailla tai etsiä tietokannassa olevista merkkijonoista kirjaimia yhtenäisellä tavalla.

## Miten

Fish Shellilla merkkijonon muuttaminen pieniksi kirjaimiksi on helppoa! Voit käyttää "string tolower" komentoa ja antaa sen parametrina haluamasi merkkijonon. 

```Fish Shell
string tolower "TÄMÄ ON MERKKIJONO" 
```

Tämän komennon tulostus on "tämä on merkkijono". Näet, että kaikki merkit ovat nyt pieniä kirjaimia.

## Syvällinen sukellus

Fish Shellin "string tolower" komento käyttää POSIX standardissa määriteltyä "tolower" -funktiota konvertoinnissa. Tämä tarkoittaa, että komento toimii samalla tavalla kaikissa POSIX-yhteensopivissa järjestelmissä.

Lisäksi merkkijonon muuttaminen pieniksi kirjaimiksi on tärkeää myös tietoturvasyistä. Yleinen käytäntö on tallentaa salasana tietokantaan pieninä kirjaimina, jotta salasanan vertailu on yhtenäistä ja turvallisempaa.

## Katso myös

- [Fish Shellin dokumentaatio merkkijonon muuttamisesta isoiksi tai pieniksi kirjaimiksi](https://fishshell.com/docs/current/cmds/string.html)
- [POSIX standardin määrittely "tolower" -funktiolle](https://pubs.opengroup.org/onlinepubs/9699919799/functions/tolower.html)
- [Merkkijonojen muuntaminen muihin kirjainmuotoihin Fish Shellilla](https://fishshell.com/docs/current/cmds/uppercase.html)