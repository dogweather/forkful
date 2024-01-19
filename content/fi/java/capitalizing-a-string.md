---
title:                "Merkkijonon muuttaminen isoiksi kirjaimiksi"
html_title:           "Java: Merkkijonon muuttaminen isoiksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen isoiksi kirjaimiksi"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Merkkijonon kapitalisointi tarkoittaa, että muutat merkkijonon ensimmäisen kirjaimen suureksi. Ohjelmoijat tekevät sen usein parantaakseen tekstien luettavuutta ja jäsentelyä.

## Kuinka:

Voit muuttaa merkkijonon ensimmäisen kirjaimen isoksi käyttämällä Java:n `substring()` ja `toUpperCase()` metodeja näin:

```Java
public class Main {

    public static String capitalize(String str) {
        if(str.isEmpty()){
            return str;
        }else{
            return str.substring(0, 1).toUpperCase() + str.substring(1).toLowerCase();
        }
    }

    public static void main(String[] args) {
        System.out.println(capitalize("hello world"));  // Outputs: Hello world
    }
}
```

## Syvempi sukellus:

Historiallisesti merkkijonojen kapitalisointi juontaa juurensa ajoista, jolloin tietokoneohjelmat kirjoitettiin kirjoituskoneilla - pääoman alkukirjain teki komentoriveistä helpommin luettavia.

Vaihtoehtoina, voit käyttää Apache Commons Stringutils-kirjastoa, joka tarjoaa `capitalize()`-metodin, mutta se lisää ylimääräisen riippuvuuden projektiisi.

Merkkijonon kapitalisoinnin toteutus voi vaihdella. Esimerkiksi jotkut ohjelmoijat saattavat valita vain ensimmäisen kirjaimen suurentamisen jättämällä loput kirjaimet muuttumattomiksi sen sijaan, että muuttavat ne pieniksi.

## Katso myös:

Voit lukea lisää Java:n `substring()` ja `toUpperCase()` metodeista Javan virallisessa dokumentaatiossa:
- [substring()](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html#substring(int,int))
- [toUpperCase()](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html#toUpperCase())

Lisätietoja merkkijonojen kapitalisoinnista yleisesti ja sen eri tekniikoista on täällä: 
- [Capitalization in Wikipedia](https://en.wikipedia.org/wiki/Capitalization)
- [Apache Commons StringUtils](https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringUtils.html)