---
date: 2024-01-26 00:53:27.481693-07:00
description: "Virheiden k\xE4sittely tarkoittaa sellaisen koodin kirjoittamista, joka\
  \ ennakoi ja k\xE4sittelee virhetilanteita. Ohjelmoijat tekev\xE4t sit\xE4 tehd\xE4\
  kseen\u2026"
lastmod: '2024-03-13T22:44:56.452388-06:00'
model: gpt-4-1106-preview
summary: "Virheiden k\xE4sittely tarkoittaa sellaisen koodin kirjoittamista, joka\
  \ ennakoi ja k\xE4sittelee virhetilanteita."
title: "Virheiden k\xE4sittely"
weight: 16
---

## Kuinka:
Java käyttää poikkeuksia virheiden käsittelyyn. Sijoitat riskialttiin koodin `try`-lohkon sisään ja otat kiinni poikkeukset `catch`-avainsanalla. Tässä on yksinkertainen esimerkki:

```java
public class ErrorHandlingExample {
    public static void main(String[] args) {
        try {
            int tulos = jaa(10, 0);
            System.out.println("Tulos on: " + tulos);
        } catch (ArithmeticException e) {
            System.out.println("Hups, nollalla ei voi jakaa!");
        }
    }

    private static int jaa(int jaettava, int jakaja) {
        return jaettava / jakaja;
    }
}
```

Tulostus:
```
Hups, nollalla ei voi jakaa!
```

## Syväsukellus
Virheenkäsittely Javassa on kehittynyt. Alkuaikoina ei ollut poikkeuksia; ohjelmoijat tarkistivat virhekoodit. Sitten Java toi käyttöön try-catch-lohkot, mahdollistaen tyylikkäämmän virheenkäsittelyn.

Vaihtoehtoja perinteiselle `try-catch` -rakenteelle ovat `try-with-resources` automaattisesti sulkeutuvia resursseja varten ja puhtaamman koodin aikaansaamiseen, otettu käyttöön Javassa 7.

Toteutuksen yksityiskohdat ovat tärkeitä. Esimerkiksi `Exception` tai `Throwable` -luokan poikkeusten ottaminen kiinni on yleensä huono käytäntö. Se on liian laaja-alainen, peittäen alleen mahdollisesti huomaamattomat virheet. Pysyttele tietyissä poikkeuksissa.

## Katso myös
- Viralliset Oracle Java -opetusmateriaalit poikkeuksista: [https://docs.oracle.com/javase/tutorial/essential/exceptions/](https://docs.oracle.com/javase/tutorial/essential/exceptions/)
- Javan `try-with-resources` -lauseen dokumentaatio: [https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html](https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html)
- Effective Java kirjoittanut Joshua Bloch, parhaista käytännöistä poikkeuksien suhteen.
