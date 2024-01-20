---
title:                "Merkkijonon muuttaminen isoiksi kirjaimiksi"
html_title:           "Arduino: Merkkijonon muuttaminen isoiksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen isoiksi kirjaimiksi"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
"Mikä & Miksi?"

Merkkijonon muotoilu isoiksi kirjaimiksi tarkoittaa, että jokainen merkkijonon kirjain muutetaan isoksi alkukirjaimeksi. Ohjelmoijat käyttävät tätä esimerkiksi kun halutaan standardoida tekstin ulkoasu tai korostaa otsikkoa.

## How to:
"Miten tehdään:"

```java
public class StringCapitalizer {
    
    public static void main(String[] args) {
        String original = "moikka maailma";
        String capitalized = original.toUpperCase();
        
        System.out.println(capitalized); // MOIKKA MAAILMA
    }
}
```

## Deep Dive
"Sukellus syvemmälle"

Stringien muotoilu isoiksi kirjaimiksi juontaa juurensa kirjoituskoneiden ja varhaisten tietokoneiden ajoista, jolloin pieniä ja suuria kirjaimia käytettiin korostamaan tai standardoimaan tekstejä. Java tarjoaa `toUpperCase()`-metodin, mutta voit myös itse toteuttaa muunnoksen käymällä läpi merkkijonon karakterit ja muuntamalla ne käyttäen `Character.toTitleCase()`-metodia yhdistettynä `Character.isWhitespace()`-metodiin sanojen alun tunnistamiseksi.

Vaihtoehtoiset menetelmät:
- `StringUtils.capitalize()` Apache Commons Lang -kirjastosta vielä yksinkertaisempien tapojen toteuttamiseksi.
- Käytä `Text`-olioita JavaFX:ssä, jos haluat esittää muotoiltua tekstiä graafisesti.

Toteutuksen yksityiskohdat:
- `toUpperCase()` ei erottele eri kieliä tai localeja, joten se saattaa toimia odottamattomasti erikoismerkkien kanssa.
- Käyttäytymistä voi säätää `Locale`-objekteilla, jos tarvitsee huomioida tietty kieli tai kulttuurillinen konteksti.

## See Also
"Katso myös"

- [Java String Documentation](https://docs.oracle.com/javase/10/docs/api/java/lang/String.html)
- [Character Documentation](https://docs.oracle.com/javase/10/docs/api/java/lang/Character.html)
- [Apache Commons Lang](https://commons.apache.org/proper/commons-lang/)