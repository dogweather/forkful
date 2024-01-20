---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Gleam: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Merkkijonojen yhdistäminen, tai "concatenation" tarkoittaa kahden tai useamman merkkijonon liittämistä yhteen. Ohjelmoijat tekevät tämän tiedon esittämiseksi helpommin luettavaan muotoon tai dynaamisten tekstien luomiseksi.

## Näin se toimii:

Java tarjoaa useita tapoja yhdistää merkkijonoja. Tässä on kolme esimerkkiä:

1. Käyttämällä '+' -operaattoria:
```Java 
String a = "Hei, "; 
String b = "maailma!";
String c = a + b; 
System.out.println(c); // Tulostaa: "Hei, maailma!"
```

2. Käyttämällä `concat()` -metodia:
```Java 
String a = "Hei, "; 
String b = "maailma!";
String c = a.concat(b); 
System.out.println(c); // Tulostaa: "Hei, maailma!"
```

3. Käyttämällä `StringBuilder` -luokkaa:
```Java
String a = "Hei, "; 
String b = "maailma!";
StringBuilder sb = new StringBuilder(a);
sb.append(b); // Liitetään b-arvo StringBuilderiin
System.out.println(sb.toString()); // Tulostaa: "Hei, maailma!"
```

## Syvällisemmin:

1. Historiallinen tausta: Merkkijonojen yhdistäminen on ollut olemassa ohjelmointikielissä jo vuosikymmeniä. Java tarjoaa useita tapoja tämän tekemiseen, jokaisella on omat vahvuudet ja heikkoutensa.

2. Vaihtoehdot: Java 8 toimitti `String.join()` -metodin, joka tarjoaa toisen tavan yhdistää merkkijonoja: `String c = String.join(" ", a, b);`

3. Toteutuksen nyanssit: Yleisimmin käytetty '+' -operaattori on melko tehokas pienillä merkkijonoilla, mutta voi olla hidas suurille merkkijonoille, koska se luo uuden merkkijonon jokaiselle concat-operaatiolle. `StringBuilder` on huomattavasti tehokkaampi suurien merkkijonojen käsittelyssä.

## Katso myös:

1. Oracle Java Docs - [String Concatenation Operator](https://docs.oracle.com/javase/specs/jls/se8/html/jls-15.html#jls-15.18.1)
2. Stack Overflow - [What is the difference between String + and String.concat()?](https://stackoverflow.com/questions/47605/string-concatenation-concat-vs-operator)
3. Baeldung - [Guide to String Concatenation in Java](https://www.baeldung.com/java-string-concatenation)