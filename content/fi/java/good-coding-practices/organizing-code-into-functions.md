---
date: 2024-01-26 01:10:56.889262-07:00
description: "Kuinka: T\xE4ss\xE4 on klassinen esimerkki - funktio luvun kertoman\
  \ laskemiselle."
lastmod: '2024-03-13T22:44:56.450572-06:00'
model: gpt-4-1106-preview
summary: "T\xE4ss\xE4 on klassinen esimerkki - funktio luvun kertoman laskemiselle."
title: "Koodin j\xE4rjest\xE4minen funktioihin"
weight: 18
---

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
