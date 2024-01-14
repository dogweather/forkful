---
title:                "Java: Tulostaminen Debug-ulosvedolla"
simple_title:         "Tulostaminen Debug-ulosvedolla"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Jos olet Java-ohjelmoija, niin olet todennäköisesti joutunut käsittelemään bugiraportteja, jotka ovat todella epämääräisiä ja sisältävät vain "ohjelma kaatuu" -ilmoituksen. Tämänkaltainen ilmoitus ei tarjoa mitään hyödyllistä tietoa, jonka avulla voit korjata ongelman nopeasti. Tässä tilanteessa debug-tekstin tulostaminen voi olla erittäin hyödyllistä diagnosoida ongelma ja löytää ratkaisu siihen.

## Miten

Debug-tekstin tulostaminen Java-ohjelmassa on helppoa. Voit käyttää "System.out.println()" -metodia, joka tulostaa halutun tekstin konsoliin. Voit myös käyttää "System.err.println()", joka tulostaa tekstin punaisella ja näin ollen erottaa sen muusta tekstistä. Tässä on yksinkertainen Java-esimerkki debug-tekstin tulostamisesta koodissa:

```Java
public class DebugExample {

    public static void main(String[] args) {
    
        // Tehdään laskutoimitus ja tulostetaan sen tulos
        int a = 5;
        int b = 2;
        int sum = a + b;
        System.out.println("Summa: " + sum);
        
        // Simuloidaan virhetilanne
        int c = 10;
        int d = 0;
        int div = c / d;
        System.err.println("Jakaminen nollalla: " + div);
    }
}
```

Tämän koodin tulos tulostettaisiin näin:

```
Summa: 7
Exception in thread "main" java.lang.ArithmeticException: / by zero
    at DebugExample.main(DebugExample.java:14)
```

Näet, että "Summa: 7" tulostuu konsoliin, mutta virheilmoitus punaisella näkyy error-tulostevirrassa.

## Syvempi sukellus

Debug-tekstin tulostaminen voi olla hyödyllistä myös silloin, kun haluat tarkastella tiettyjen arvojen tilaa suorituskyvyn tai tehokkuuden parantamiseksi. Voit esimerkiksi tulostaa tietyn muuttujan arvon ja suorittaa ohjelmaasi eri syötteillä, jotta näet, miten muuttujan arvo muuttuu jokaisen suorituksen välillä.

On myös tärkeää muistaa, että kaikkea debug-tekstiä ei tarvitse poistaa valmiista koodista. Kun olet ratkaissut ongelman ja ohjelmasi toimii odotetulla tavalla, voit poistaa debug-tulostukset tai kommentoida ne pois koodistasi, jotta se pysyy siistinä ja helpommin luettavissa.

## Katso myös

- [Java Debugging Tutorial](https://www.jetbrains.com/help/idea/debugging-your-first-java-application.html)
- [Debugging Tips and Tricks](https://blog.jetbrains.com/idea/2018/07/debugging-tips-tricks/)
- [Java Debugging with Eclipse](https://www.eclipse.org/community/eclipse_newsletter/2015/june/article3.php)