---
date: 2024-01-26 03:45:11.399172-07:00
description: "Numeroiden py\xF6rist\xE4minen tarkoittaa niiden s\xE4\xE4t\xE4mist\xE4\
  \ tiettyyn tarkkuusasteeseen. Ohjelmoijat tekev\xE4t sen yksinkertaistaakseen numeroita\
  \ luettavuuden\u2026"
lastmod: '2024-03-13T22:44:56.439819-06:00'
model: gpt-4-0125-preview
summary: "Numeroiden py\xF6rist\xE4minen tarkoittaa niiden s\xE4\xE4t\xE4mist\xE4\
  \ tiettyyn tarkkuusasteeseen. Ohjelmoijat tekev\xE4t sen yksinkertaistaakseen numeroita\
  \ luettavuuden\u2026"
title: "Numerojen py\xF6rist\xE4minen"
weight: 13
---

## Mitä ja miksi?
Numeroiden pyöristäminen tarkoittaa niiden säätämistä tiettyyn tarkkuusasteeseen. Ohjelmoijat tekevät sen yksinkertaistaakseen numeroita luettavuuden vuoksi, täyttääkseen tiettyjä vaatimuksia tai varmistaakseen, että laskelmat mahtuvat tiettyihin rajoituksiin, kuten välttääkseen tarkkuusvirheitä liukulukuaritmetiikassa.

## Miten:
Java tarjoaa useita tapoja pyöristää numeroita. Tässä nopea demo `Math.round()`, `BigDecimal` ja `DecimalFormat` käytöstä.

```java
public class RoundingDemo {
    public static void main(String[] args) {
        double num = 123.4567;

        // Käyttäen Math.round()
        long roundedNum = Math.round(num);
        System.out.println(roundedNum); // Tuloste: 123

        // Käyttäen BigDecimalia lisäkontrolliin
        BigDecimal bd = new BigDecimal(num).setScale(2, RoundingMode.HALF_UP);
        double roundedBigDecimal = bd.doubleValue();
        System.out.println(roundedBigDecimal); // Tuloste: 123.46

        // Käyttäen DecimalFormatia
        DecimalFormat df = new DecimalFormat("#.##");
        String formattedNum = df.format(num);
        System.out.println(formattedNum); // Tuloste: 123.46
    }
}
```

## Syväsukellus
Historiallisesti numeroiden pyöristäminen on ollut olennaista analogisille laskelmille ja on siirtynyt digitaaliseen laskentaan tehokkuuden ja tarkkuuden vuoksi. Pyöristysvirheet, kuten ne, jotka tulevat liukulukuaritmetiikasta, osoittavat, että tämä ei ole triviaali asia — ne voivat kumulatiivisesti sotkea laskelmia esimerkiksi ilmailu- ja rahoitussovelluksissa.

`Math.round()` lisäksi sinulla on `BigDecimal`, joka tarjoaa sinulle hienovaraisemman kontrollin skaalan ja pyöristystilan suhteen, ja `DecimalFormat` silloin, kun tarvitset pyöristää numeroita osana tekstin muotoilua. Pyöristämisen vaihtoehtoihin kuuluvat lattiafunktio, kattofunktio ja katkaisu, jotka ovat erilaisia tapoja käsitellä tarkkuutta ja tyypillisesti käsitellään eri `Math` menetelmillä.

Käyttötapauksestasi riippuen pyöristysstrategia saattaa vaihdella. Esimerkiksi `BigDecimal` on go-to rahoituslaskelmissa, joissa tarkkuus on kriittistä. Sen sijaan `Math.round()` on nopea tapa yleiskäyttöisiin toimenpiteisiin, joissa et ole niin tarkka pyöristystilasta.

## Katso myös
- [Oraclen Java Math dokumentaatio](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/Math.html)
- [IEEE-standardi liukulukuaritmetiikalle (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)
- [DecimalFormat-luokka Javassa](https://docs.oracle.com/javase/7/docs/api/java/text/DecimalFormat.html)
