---
date: 2024-01-26 01:10:56.889262-07:00
description: "Koodin j\xE4rjest\xE4minen funktioihin tarkoittaa ohjelman jakamista\
  \ hallittaviin palasiin, joista kukin suorittaa oman, erillisen teht\xE4v\xE4ns\xE4\
  . Ohjelmoijat\u2026"
lastmod: '2024-02-25T18:49:53.372336-07:00'
model: gpt-4-1106-preview
summary: "Koodin j\xE4rjest\xE4minen funktioihin tarkoittaa ohjelman jakamista hallittaviin\
  \ palasiin, joista kukin suorittaa oman, erillisen teht\xE4v\xE4ns\xE4. Ohjelmoijat\u2026"
title: "Koodin j\xE4rjest\xE4minen funktioihin"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Koodin järjestäminen funktioihin tarkoittaa ohjelman jakamista hallittaviin palasiin, joista kukin suorittaa oman, erillisen tehtävänsä. Ohjelmoijat tekevät tämän tehdäkseen koodista luettavaa, uudelleenkäytettävää ja ylläpidettävää.

## Kuinka:
Tässä on klassinen esimerkki - funktio luvun kertoman laskemiselle.

```java
public class MathUtils {

    public static void main(String[] args) {
        int numero = 5;
        int tulos = factorial(numero);
        System.out.println("Luvun " + numero + " kertoma on: " + tulos);
    }
    
    public static int factorial(int n) {
        if (n <= 1) {
            return 1;
        }
        return n * factorial(n - 1);
    }
}
```

Tuloste olisi:
```
Luvun 5 kertoma on: 120
```

## Syväsukellus
Ennen funktioita, koodia ahdettiin monoliittisiin lohkoihin, mikä teki vikojen etsimisestä kuin neulan etsimistä heinäsuovasta. Nyt funktioiden eriyttäminen auttaa paikallistamaan ongelmia nopeasti. Vaihtoehtoja ovat mm. lambda-lausekkeet Javassa tai menetelmät oliopohjaisessa ohjelmoinnissa, jotka molemmat palvelevat samankaltaisia tarkoituksia. Kun kirjoitat funktion, muista: (1) Jokaisella funktiolla tulisi olla yksi vastuualue, ja (2) funktion nimen tulisi selkeästi kuvailla sen tarkoitusta.

## Katso myös
Lisää tietoa koodin järjestämisestä:
- Clean Code, kirjoittanut Robert C. Martin
- Refactoring: Improving the Design of Existing Code, kirjoittanut Martin Fowler
- [Oracle Java -dokumentaatio menetelmien määrittämisestä](https://docs.oracle.com/javase/tutorial/java/javaOO/methods.html)
