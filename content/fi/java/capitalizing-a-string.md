---
title:    "Java: Merkkijonon kirjoittaminen isoilla kirjaimilla"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Miksi

Miksi on tärkeää osata muuttaa merkkijonon kirjaimet isoihin tai pieniin kirjaimiin? Yksi syy voisi olla tiettyjen käyttäjätietojen validointi, esimerkiksi käyttäjänimen tarkistaminen ilman, että isot ja pienet kirjaimet vaikuttavat tulokseen.

## Miten

Java-ohjelmointikielessä on valmiina metodeja merkkijonon muuntamiseen, joten tämä tehtävä ei ole vaikea. Tässä on yksinkertainen esimerkki, joka muuttaa merkkijonon ensimmäisen kirjaimen isoksi.

```Java
String s = "tämä on vain esimerkki";
String capitalizedString = s.substring(0, 1).toUpperCase() + s.substring(1).toLowerCase();

System.out.println(capitalizedString);
```

Tuloste: Tämä on vain esimerkki

Suoraviivaisesti tämä koodinpätkä ensin muuttaa merkkijonon ensimmäisen kirjaimen isoksi `toUpperCase()` -methodilla ja sitten muuttaa loput kirjaimet pieniksi `toLowerCase()` -methodilla. Mutta entä jos merkkijonossa on monia sanoja tai merkkijonossa on joitakin erikoismerkkejä? Tässä tapauksessa voisi olla parempi käyttää `split()`- methodia, joka jakaa merkkijonon sanoiksi ja sitten muuntaa ensimmäisen kirjaimen isoksi.

```Java
String s = "tämä on toinen esimerkki!";
String[] words = s.split(" ");
String capitalizedString = "";

for (String word : words) {
    String capitalizedWord = word.substring(0, 1).toUpperCase() + word.substring(1).toLowerCase();
    capitalizedString += capitalizedWord + " ";
}

System.out.println(capitalizedString);
```

Tuloste: Tämä On Toinen Esimerkki!

## Syvemmälle

Merkkijonon muuntaminen on tärkeä osa ohjelmoinnin perusteita. Se auttaa meitä varmistamaan, että käyttäjän antamat tiedot ovat oikeassa muodossa ja helpottaa tietojen käsittelyä. Muuntaminen isoihin tai pieniin kirjaimiin voi joskus olla myös osa ohjelman toiminnallisuutta, esimerkiksi jos haluamme vertailla merkkijonoja ilman, että isot ja pienet kirjaimet vaikuttavat tulokseen. On hyvä osata erilaisia tapoja muuttaa merkkijonoa eri tilanteisiin sopivaksi.

## Katso myös

- [Java String Documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [TutorialsPoint: Java - Changing the Case of Characters](https://www.tutorialspoint.com/java/string_touppercase.htm)
- [GeeksforGeeks: String - toUpperCase() and toLowerCase() methods in Java](https://www.geeksforgeeks.org/java-string-touppercase-method-example/)