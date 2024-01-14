---
title:    "Java: Merkkijonon pituuden löytäminen"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Miksi

On monia käytännön syitä, miksi saattaisit haluta löytää merkkijonon pituuden Java-ohjelmoinnissa. Ehkä haluat varmistaa, että käyttäjä ei syötä liian monta merkkiä tekstikenttään tai ehkä haluat tietää, kuinka monta merkkiä tietyssä lauseessa on. Joka tapauksessa, tieto merkkijonon pituudesta on hyödyllinen tieto, jota voit käyttää ohjelmasi toiminnan täydentämiseen.

## Kuinka

Merkkijonon pituuden löytäminen Java-ohjelmoinnissa on yksinkertaista. Voit käyttää String-luokan length-metodia. Alla on esimerkki koodista:

```Java
String sana = "Tervetuloa";
int pituus = sana.length();
System.out.println("Merkkijonon \"" + sana + "\" pituus on " + pituus + " merkkiä.");
```

Tämä koodi tallentaa ensin merkkijonon "Tervetuloa" muuttujaan sana ja sitten käyttää length-metodia laskemaan sen pituuden. Lopuksi tulostamme tiedon merkkijonon pituudesta näytölle. Suoritettaessa tämä koodi antaa seuraavan outputin:

```
Merkkijonon "Tervetuloa" pituus on 11 merkkiä.
```

## Syväsukellus

String-luokka on olennainen osa Java-ohjelmointia ja sillä on paljon hyödyllisiä metodeja, jotka voit oppia. Length-metodin lisäksi voit esimerkiksi käyttää concat-metodia kahden merkkijonon yhdistämiseen tai equals-metodia kahden merkkijonon vertaamiseen. Kannattaa tutustua tarkemmin String-luokan dokumentaatioon, jotta voit valjastaa sen voiman omiin ohjelmiisi!

## Katso myös

- [String-luokan dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java-ohjelmointikurssi](https://www.java.com/en/)
- [Merkkijonon käsittelyn perusteet Java-ohjelmoinnissa](https://www.geeksforgeeks.org/java-string-programs-set-1/)