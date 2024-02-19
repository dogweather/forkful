---
aliases:
- /fi/kotlin/working-with-complex-numbers/
date: 2024-01-26 04:43:44.149456-07:00
description: "Kompleksiluvut laajentavat lukuj\xE4rjestelm\xE4\xE4mme sis\xE4lt\xE4\
  m\xE4\xE4n negatiivisten lukujen neli\xF6juuret, miss\xE4 'imaginaariyksikk\xF6\
  ' i on -1:n neli\xF6juuri. Ohjelmoijat\u2026"
lastmod: 2024-02-18 23:09:07.568521
model: gpt-4-0125-preview
summary: "Kompleksiluvut laajentavat lukuj\xE4rjestelm\xE4\xE4mme sis\xE4lt\xE4m\xE4\
  \xE4n negatiivisten lukujen neli\xF6juuret, miss\xE4 'imaginaariyksikk\xF6' i on\
  \ -1:n neli\xF6juuri. Ohjelmoijat\u2026"
title: "Kompleksilukujen k\xE4sittely"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Kompleksiluvut laajentavat lukujärjestelmäämme sisältämään negatiivisten lukujen neliöjuuret, missä 'imaginaariyksikkö' i on -1:n neliöjuuri. Ohjelmoijat käyttävät niitä aloilla kuten insinööritiede, fysiikka ja signaalinkäsittely, koska ne ovat erinomaisia aaltojen, värähtelyiden ja kaiken pyörivän mallintamisessa.

## Miten:

Määritellään perus kompleksilukuluokka Kotlinissa:

```kotlin
data class Complex(val real: Double, val imaginary: Double) {
    operator fun plus(other: Complex) = Complex(real + other.real, imaginary + other.imaginary)
    operator fun minus(other: Complex) = Complex(real - other.real, imaginary - other.imaginary)
    operator fun times(other: Complex) = Complex(
        real * other.real - imaginary * other.imaginary,
        real * other.imaginary + imaginary * other.real
    )
    
    override fun toString(): String = "($real + ${imaginary}i)"
}

fun main() {
    val a = Complex(1.0, 2.0)
    val b = Complex(3.0, 4.0)
    
    println("a + b = ${a + b}")  // Tuloste: a + b = (4.0 + 6.0i)
    println("a - b = ${a - b}")  // Tuloste: a - b = (-2.0 - 2.0i)
    println("a * b = ${a * b}")  // Tuloste: a * b = (-5.0 + 10.0i)
}
```

## Syväsukellus

Kompleksiluvuista mainittiin ensimmäisen kerran 1500-luvulla, ratkaisten kuutiollisia yhtälöitä, joilla ei ollut reaaliluku ratkaisuja. Insinööritiede ja fysiikka hyötyvät suuresti kompleksiluvuista analysoidessaan vaihtovirtapiirejä ja aaltomuotoja. Raskaampaan työhön voi vaihtoehtoisesti käyttää kirjastoa kuten Kotlinin `koma` tai `ejml`.

Kompleksilukujen operaatiot peilaavat reaalilukuja, mutta huomioiden imaginaariyksikön. Kertolasku esimerkiksi noudattaa distribuutiolakia, muistaen että `i^2 = -1`. Tämä imaginaariyksikkö mahdollistaa meidän edustavan moniulotteisia lukuja, mikä on kriittistä erilaisissa tieteellisissä laskelmissa.

## Katso Myös

Kotlin Matematiikkakirjastot:

- [koma](https://koma.kyonifer.com/): Tieteellinen laskentakirjasto Kotlinille.

Lisäluettavaa kompleksiluvuista:

- [Wikipedia: Kompleksiluvut](https://en.wikipedia.org/wiki/Complex_number)
