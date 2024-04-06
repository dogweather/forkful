---
date: 2024-01-20 17:53:00.540390-07:00
description: "How to: (Kuinka tehd\xE4:) Alkujaan, konsoliin tulostaminen oli yksi\
  \ harvoista tavoista saada v\xE4lit\xF6nt\xE4 palautetta ohjelman suorituksesta.\
  \ Lukuisat\u2026"
lastmod: '2024-04-05T22:51:10.598689-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4:) Alkujaan, konsoliin tulostaminen oli yksi harvoista tavoista\
  \ saada v\xE4lit\xF6nt\xE4 palautetta ohjelman suorituksesta."
title: "Virheenj\xE4ljitystulosteiden tulostaminen"
weight: 33
---

## How to: (Kuinka tehdä:)
```java
public class DebugDemo {
    public static void main(String[] args) {
        int luku = 42;
        System.out.println("Muuttujan 'luku' arvo on: " + luku);
        
        // Kokeillaan, toimiiko silmukka oikein
        for(int i = 0; i < 5; i++) {
            System.out.println("Silmukan iteraatio: " + i);
        }
        
        // Tsekataan, palauttaako funktio oikean tuloksen
        int tulos = kertolasku(7, 6);
        System.out.println("Funktion palauttama tulos: " + tulos);
    }
    
    public static int kertolasku(int a, int b) {
        // Debug-tulostus funktion sisällä
        System.out.println("Kertolasku, luvut: " + a + " ja " + b);
        return a * b;
    }
}
```
Sample output:
```
Muuttujan 'luku' arvo on: 42
Silmukan iteraatio: 0
Silmukan iteraatio: 1
Silmukan iteraatio: 2
Silmukan iteraatio: 3
Silmukan iteraatio: 4
Kertolasku, luvut: 7 ja 6
Funktion palauttama tulos: 42
```

## Deep Dive (Syväsukellus)
Alkujaan, konsoliin tulostaminen oli yksi harvoista tavoista saada välitöntä palautetta ohjelman suorituksesta. Lukuisat debuggertyökalut ovat syntyneet, mutta `System.out.println` pysyy käytössä sen yksinkertaisuuden ja nopeuden vuoksi. Vaihtoehtoina ovat esimerkiksi loggervälineet (kuten Log4j), jotka tarjoavat hienostuneempaa kontrollia ja tietojen tallennusta. Koodin sisäiset debug-tulosteet kannattaa poistaa tuotantoversiosta suorituskyvyn ja turvallisuuden takia.

## See Also (Katso myös)
- [Oracle Java Documentation](https://docs.oracle.com/en/java/)
- [Log4j – Apache Logging Services](https://logging.apache.org/log4j/2.x/)
- [Java Debugging with Eclipse - Tutorial](http://www.vogella.com/tutorials/EclipseDebugging/article.html)
