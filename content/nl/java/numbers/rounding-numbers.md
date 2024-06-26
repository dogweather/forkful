---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:53.655956-07:00
description: 'Hoe te: Java biedt meerdere manieren om getallen af te ronden. Hier
  is een snelle demo met `Math.round()`, `BigDecimal`, en `DecimalFormat`.'
lastmod: '2024-03-13T22:44:50.676263-06:00'
model: gpt-4-0125-preview
summary: Java biedt meerdere manieren om getallen af te ronden.
title: Afronden van getallen
weight: 13
---

## Hoe te:
Java biedt meerdere manieren om getallen af te ronden. Hier is een snelle demo met `Math.round()`, `BigDecimal`, en `DecimalFormat`.

```java
public class RoundingDemo {
    public static void main(String[] args) {
        double num = 123.4567;

        // Gebruik makend van Math.round()
        long roundedNum = Math.round(num);
        System.out.println(roundedNum); // Uitvoer: 123

        // Gebruik makend van BigDecimal voor meer controle
        BigDecimal bd = new BigDecimal(num).setScale(2, RoundingMode.HALF_UP);
        double roundedBigDecimal = bd.doubleValue();
        System.out.println(roundedBigDecimal); // Uitvoer: 123.46

        // Gebruik makend van DecimalFormat
        DecimalFormat df = new DecimalFormat("#.##");
        String formattedNum = df.format(num);
        System.out.println(formattedNum); // Uitvoer: 123.46
    }
}
```

## Diepgaande Verkenning
Historisch gezien is het afronden van getallen essentieel geweest voor analoge berekeningen en is overgegaan op digitale computing voor efficiëntie en nauwkeurigheid. Afrondingsfouten, zoals die van floating-point rekenkunde, demonstreren dat dit geen triviaal probleem is — ze kunnen opeenstapeling de berekeningen in bijvoorbeeld de lucht- en ruimtevaart en financiële toepassingen in de war sturen.

Buiten `Math.round()`, heb je `BigDecimal`, wat je fijnere controle geeft over de schaal en afrondingsmodus, en `DecimalFormat` voor wanneer je getallen moet afronden als onderdeel van het formatteren van tekstuitvoer. Alternatieven voor afronden omvatten floor, ceiling, en truncating, wat verschillende manieren zijn om met precisie om te gaan en typisch behandeld worden door diverse `Math` methodes.

Afhankelijk van jouw gebruikssituatie, kan de afrondingsstrategie variëren. Bijvoorbeeld, `BigDecimal` is de go-to voor financiële berekeningen, waar precisie cruciaal is. In tegenstelling, is `Math.round()` een snelle manier voor algemene operaties waar je minder kieskeurig bent over de afrondingsmodus.

## Zie Ook
- [Oracle's Java Math documentatie](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/Math.html)
- [IEEE Standaard voor Floating-Point Rekenkunde (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)
- [DecimalFormat Klasse in Java](https://docs.oracle.com/javase/7/docs/api/java/text/DecimalFormat.html)
