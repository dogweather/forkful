---
title:                "Kompleksilukujen käsittely"
date:                  2024-01-26T04:41:52.714525-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kompleksilukujen käsittely"

category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Kompleksiluvut laajentavat reaalilukusuoraa lisäämällä imaginaariyksikön, `i`, missä `i^2 = -1`. Ne ovat keskeisiä aloilla kuten insinööritiede, fysiikka ja edistynyt matematiikka, missä ne mallintavat ilmiöitä, joita reaaliluvut eivät voi käsitellä, kuten sähkövirtoja ja signaalinkäsittelyä.

## Kuinka:

Javalla ei ole sisäänrakennettua tukea kompleksiluvuille, mutta voimme luoda oman luokkamme tai käyttää kirjastoa. Tässä on nopea esimerkki siitä, miten luoda yksinkertainen `ComplexNumber` luokka ja käyttää sitä:

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

    // ToString näyttääkseen kompleksiluvut a + bi muodossa
    @Override
    public String toString() {
        return String.format("%.1f + %.1fi", real, imaginary);
    }

    // Nopea testi
    public static void main(String[] args) {
        ComplexNumber c1 = new ComplexNumber(2, 3);
        ComplexNumber c2 = new ComplexNumber(1, 4);

        System.out.println("Summa: " + c1.add(c2));
    }
}
```

Esimerkkituloste päämetodille on:

```
Summa: 3.0 + 7.0i
```

## Syväluotaus

Ennen korkeamman tason kieliä, kuten Javaa, ohjelmoijat työskentelivät suoraan matematiikkakirjastojen kanssa kielissä, kuten Fortran tai C, hallitakseen kompleksisia operaatioita. Konsepti juontaa juurensa 1500-luvulle, ja sen kreditoivat matemaatikot kuten Gerolamo Cardano ja Rafael Bombelli.

Javassa, `java.lang.Math` on menotapa oleellisiin, mutta se jättää kompleksiluvut huomiotta, todennäköisesti koska jokainen ohjelmoija ei käytä niitä. Vaihtoehdot? Käytä kirjastoja. Apache Commons Math tarjoaa `Complex` luokan, joka on pakattu metodeilla manipulointia varten. Tässä on kuitenkin syy, miksi oman luokan tekeminen on siistiä: Kevyt, räätälöity tarkasti tarpeisiisi, eikä kirjaston ylikuormitusta.

Yksi tärkeä yksityiskohta: varo liukulukutarkkuutta. Tietokoneet eivät pysty esittämään joitakin numeroita tarkasti, mikä johtaa pyöristysvirheisiin. Suorittaessa toistuvia kompleksisia operaatioita, nämä virheet voivat kumuloitua!

## Katso Myös

Syvempiin sukelluksiin ja monimutkaisempiin operaatioihin, tutki:

- [Apache Commons Math](https://commons.apache.org/proper/commons-math/)
- [JSciencen Complex luokka](http://jscience.org/)
- Oraclen tutoriaalit [liukulukuaritmetiikasta](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
