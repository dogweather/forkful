---
title:                "Java: Palaavien alimerkkien erottaminen"
simple_title:         "Palaavien alimerkkien erottaminen"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/extracting-substrings.md"
---

{{< edit_this_page >}}

# Miksi: Miksi kannattaisi tutustua alakierteisiin?

Alakierteiden eli alaryhmien erotteleminen merkkijonosta voi olla hyödyllistä monessa eri tilanteessa. Esimerkiksi jos haluat tarkistaa, esiintyykö tietty sana merkkijonossa, tai jos haluat muuttaa tiettyään osaa merkkijonosta toiseksi. Alakierreiden hyödyntäminen voi myös tehdä koodista lyhyemmän ja helpommin ymmärrettävän.

# Miten: Esimerkkejä alakierteiden hyödyntämisestä Java-kielellä.

```Java
String teksti = "Tervetuloa Suomeen!";
String alakierte1 = teksti.substring(0, 9); // "Tervetulo"
String alakierte2 = teksti.substring(10); // "Suomeen!"
System.out.println(alakierte1);
System.out.println(alakierte2);
```

Yllä olevassa esimerkissä käytetään merkkijonon `substring()` -metodia, joka ottaa parametreikseen alku- ja loppuindeksin. Tämän avulla voidaan erottaa alkuperäisestä merkkijonosta haluttu alakierte. Toinen parametri on valinnainen, ja mikäli se jätetään pois, otetaan alakierteenä kaikki merkit annetusta indeksistä eteenpäin.

```Java
String sana = "koodaus";
String ensimmainenKirjain = sana.substring(0, 1); // "k"
String kaikkiMuutKirjaimet = sana.substring(1); // "oodaus"
System.out.println(ensimmainenKirjain);
System.out.println(kaikkiMuutKirjaimet);
```

Toisessa esimerkissä haluamme erottaa sana `koodaus` kahteen alakierteeseen: ensimmmäiseen kirjaimeen ja kaikkiin muihin kirjaimiin. Huomaa, että indeksit alkavat aina nollasta, joten kirjain `k` on indeksillä 0.

# Syvempää tietoa alakierteistä

Alakierteiden hyödyntäminen on yksi tapa käsitellä merkkijonoja Java-ohjelmoinnissa. Metodia `substring()` voi käyttää myös monilla muilla tavoilla, kuten esimerkiksi antamalla parametrina vain yhden indeksin, jolloin halutuksi alakierteeksi tulee kaikki merkit kyseisestä indeksistä eteenpäin. Voidaan myös määrittää negatiivisia indeksejä, jotka aloittavat laskemisen merkkijonon lopusta.

Alakierteitä voidaan myös yhdistää muihin merkkijonon manipulointimetodeihin, kuten `concat()`, `replace()` ja `charAt()`. Niiden avulla voidaan luoda monimutkaisempia alakierteitä ja muokata niitä halutulla tavalla.

# Katso myös

- Java String -dokumentaatio: https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html
- Metodi `substring()` -esimerkit: https://www.w3schools.com/java/ref_string_substring.asp
- "Mikä ihmeen alakierte?" -blogipostaus: https://javafm.fi/posts/alakierte/