---
title:                "Numerojen pyöristäminen"
aliases:
- /fi/java/rounding-numbers.md
date:                  2024-01-26T03:45:11.399172-07:00
model:                 gpt-4-0125-preview
simple_title:         "Numerojen pyöristäminen"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/rounding-numbers.md"
---

{{< edit_this_page >}}

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
