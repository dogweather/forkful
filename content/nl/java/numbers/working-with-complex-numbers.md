---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:39.199677-07:00
description: "Complexe getallen breiden de re\xEBle getallenlijn uit door de toevoeging\
  \ van een imaginair eenheid, `i`, waarbij `i^2 = -1`. Ze zijn cruciaal in velden\u2026"
lastmod: '2024-02-25T18:49:48.017755-07:00'
model: gpt-4-0125-preview
summary: "Complexe getallen breiden de re\xEBle getallenlijn uit door de toevoeging\
  \ van een imaginair eenheid, `i`, waarbij `i^2 = -1`. Ze zijn cruciaal in velden\u2026"
title: Werken met complexe getallen
---

{{< edit_this_page >}}

## Wat & Waarom?

Complexe getallen breiden de reële getallenlijn uit door de toevoeging van een imaginair eenheid, `i`, waarbij `i^2 = -1`. Ze zijn cruciaal in velden zoals techniek, natuurkunde en geavanceerde wiskunde, waar ze verschijnselen modelleren die niet met reële getallen kunnen worden afgehandeld, zoals elektrische stromen en signaalverwerking.

## Hoe te:

Java heeft geen ingebouwde ondersteuning voor complexe getallen, maar we kunnen onze eigen klasse bouwen of een bibliotheek gebruiken. Hier is een snel voorbeeld van hoe je een eenvoudige `ComplexNumber` klasse kunt maken en gebruiken:

```java
public class ComplexNumber {
    private double real;
    private double imaginary;

    public ComplexNumber(double real, double imaginary) {
        this.real = real;
        this.imaginary = imaginary;
    }

    public ComplexNumber add(ComplexNumber other) {
        return new ComplexNumber(this.real + other.real, this.imaginary + other.imaginary);
    }

    // ToString om complexe getallen weer te geven in a + bi vorm
    @Override
    public String toString() {
        return String.format("%.1f + %.1fi", real, imaginary);
    }

    // Snelle test
    public static void main(String[] args) {
        ComplexNumber c1 = new ComplexNumber(2, 3);
        ComplexNumber c2 = new ComplexNumber(1, 4);

        System.out.println("Som: " + c1.add(c2));
    }
}
```

Voorbeelduitvoer voor de hoofdmethode zal zijn:

```
Som: 3.0 + 7.0i
```

## Diepere Duik

Voordat hoogtalen zoals Java bestonden, werkten programmeurs rechtstreeks met wiskundige bibliotheken in talen zoals Fortran of C om complexe operaties te beheren. Het concept gaat terug tot de 16e eeuw, toegeschreven aan wiskundigen zoals Gerolamo Cardano en Rafael Bombelli.

In Java is `java.lang.Math` de plek voor essentiële zaken, maar slaat compleze getallen over, waarschijnlijk omdat niet elke programmeur ze gebruikt. Alternatieven? Gebruik bibliotheken. Apache Commons Math biedt een `Complex` klasse verpakt met methoden voor manipulatie. Hier is echter waarom het zelf bouwen netjes is: Lichtgewicht, op maat gemaakt voor je exacte behoeften, en geen bibliotheek overhead.

Eén belangrijk detail: let op drijvende komma precisie. Computers kunnen sommige getallen niet exact voorstellen, wat leidt tot afrondingsfouten. Bij het uitvoeren van herhaalde complexe bewerkingen kunnen deze fouten zich opstapelen!

## Zie Ook

Voor diepere duiken en meer complexe handelingen, zie:

- [Apache Commons Math](https://commons.apache.org/proper/commons-math/)
- [JScience's Complex klasse](http://jscience.org/)
- Oracle's tutorials over [floating-point arithmetic](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
