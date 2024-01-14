---
title:    "Java: Merkkijonon pituuden löytäminen"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit selvittää merkkijonon pituuden? Yksi yleinen syy on, että se on osa monimutkaisempaa ohjelmaa, joka vaatii tietyn merkkijonon pituuden laskemista. Tämä voi olla esimerkiksi tärkeää tekstianalyysin tai datan käsittelyn kannalta.

## Miten

Java tarjoaa yksinkertaisen tavan selvittää merkkijonon pituus käyttämällä String-luokan length()-metodia. Tämä metodi palauttaa merkkijonon pituuden eli merkkien määrän. Alla on esimerkki Java-koodista, jossa lasketaan merkkijonon "Tervetuloa" pituus ja tulostetaan se konsoliin:

```Java
String teksti = "Tervetuloa";
System.out.println(teksti.length());

// Output: 10
```

Merkkijonon pituus on siis 10 merkkiä.

## Syvempi sukellus

Merkkijonot ovat olennainen osa ohjelmointia, ja Java tarjoaa monia hyödyllisiä metodeja niiden käsittelyyn. Jokaisella merkkijonolla on oma pituus, joka voi vaihdella tilanteesta riippuen. Esimerkiksi tyhjä merkkijono on pituudeltaan 0, kun taas Unicode-merkkijonolla voi olla huomattavasti suurempi pituus kuin sen näkyvä tulostus. On myös hyvä muistaa, että merkkijonon pituutta ei tule sekoittaa sen indeksin kanssa. Indeksi kertoo, missä kohtaa merkkijonoa tietty merkki sijaitsee, kun taas pituus kertoo vain merkkijonon kokonaispituuden.

Jos haluat tutustua syvemmin merkkijonojen käsittelyyn Javassa, voit tutustua Java String API:n dokumentaatioon. Sieltä löydät kaikki tarvittavat tiedot ja tarkemmat esimerkit merkkijonojen pituuden laskemisesta ja muista käsittelymetodeista.

## Katso myös

- [Java String API](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Merkkijonot Javassa](https://www.javatpoint.com/java-string)
- [Java-merkkijonon pituuden laskeminen](https://stackabuse.com/get-string-length-in-java/)