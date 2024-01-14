---
title:    "Java: Säännöllisten lausekkeiden käyttö"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita Java-ohjelmoinnissa?

Säännölliset lausekkeet ovat erittäin hyödyllisiä työkaluja, kun halutaan käsitellä tekstipohjaista dataa Java-ohjelman sisällä. Ne mahdollistavat monimutkaisempien hakutoimintojen suorittamisen lyhyillä ja tehokkailla koodinpätkillä. Käyttämällä säännöllisiä lausekkeita voit esimerkiksi etsiä ja korvata tiettyjä merkkijonoja tai suodattaa tietoja haluamallasi tavalla.

## Miten käyttää säännöllisiä lausekkeita Java-ohjelmoinnissa?

Säännöllisten lausekkeiden käyttö Java-ohjelmoinnissa tapahtuu pääasiassa java.util.regex-paketin kautta. Voit aloittaa käyttämällä säännöllisiä lausekkeita luomalla Pattern-objektin, joka ottaa vastaan säännöllisen lausekkeen ja halutun merkkijonon. Tämän jälkeen voit käyttää Matcher-objektia suorittamaan haluamasi toiminnot. Alla on yksinkertainen esimerkki:

```Java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegularExpressions {

    public static void main(String[] args) {
        String inputString = "Tämä on esimerkki tekstipohjaisesta datasta";
        String regex = "esimerkki";
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(inputString);
        while (matcher.find()) {
            System.out.println("Löytyi osumat: " + matcher.group());
        }
    }
}
```

Koodin tulos:

```
Löytyi osumat: esimerkki
```

Tämä yksinkertainen esimerkki etsii annetusta merkkijonosta säännöllisen lausekkeen "esimerkki" ja tulostaa kaikki löydetyt osumat.

## Syvempää tietoa säännöllisten lausekkeiden käytöstä

Säännölliset lausekkeet ovat hyvin monipuolisia ja niillä voi suorittaa monenlaisia toimintoja, kuten esimerkiksi metakarakterien käyttöä, jolla voit hakea merkkijonoja tiettyjen kriteerien mukaan. Voit myös käyttää säännöllisiä lausekkeita merkkijonojen muotoiluun ja validointiin. On tärkeää tutustua säännöllisten lausekkeiden eri ominaisuuksiin ja toimintoihin, jotta voit hyödyntää niitä tehokkaasti Java-ohjelmoinnissa.

## Katso myös

- Java Regular Expressions Tutorial: https://www.vogella.com/tutorials/JavaRegularExpressions/article.html
- Regular Expressions Cheat Sheet: https://www.rexegg.com/regex-quickstart.html