---
title:                "Merkkijonojen osien poimiminen"
date:                  2024-01-20T17:45:57.639063-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonojen osien poimiminen"

category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä & Miksi?)
Substringit ovat jonoista irrotettuja osia. Ohjelmoijat käyttävät niitä tiedon käsittelyyn ja analysointiin - leikaten tarpeelliset palat isommasta tekstistä.

## How to: (Kuinka tehdään:)
```java
public class SubstringExample {
    public static void main(String[] args) {
        String text = "Ohjelmointi on hauskaa";
        String sub = text.substring(13); // Otetaan osa jälkeen indexin 13
        String subWithEnd = text.substring(0, 13); // Otetaan osa indeksien 0 ja 13 väliltä

        System.out.println(sub); // Tulostaa "hauskaa"
        System.out.println(subWithEnd); // Tulostaa "Ohjelmointi on"
    }
}
```
## Deep Dive (Sukellus syvyyksiin):
Substringien konsepti on vanha, juontuen ajoista kun ohjelmistoja kirjoitettiin nauhoille. Java tarjoaa `substring()` metodin `String`-luokassa, jolla voi hakea osajonot tehokkaasti. Alternatiiveina ovat `subSequence()` käyttäminen tai regex (säännölliset lausekkeet), joilla voi hakea monimutkaisempia kuvioita. Suorituskyvyn kannalta Java käyttää `String`-luokassa muistiystävällistä tapaa, jossa osajonot viittaavat alkuperäisen merkkijonon muistialueeseen eivätkä kopioi dataa suotta.

## See Also (Katso myös):
- Oracle Java Documentation: [String.substring](https://docs.oracle.com/javase/10/docs/api/java/lang/String.html#substring(int,int))
- Regular Expressions in Java: [Pattern](https://docs.oracle.com/javase/10/docs/api/java/util/regex/Pattern.html)
