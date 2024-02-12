---
title:                "Trabalhando com números complexos"
aliases:
- /pt/kotlin/working-with-complex-numbers/
date:                  2024-01-26T04:42:52.821052-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com números complexos"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Números complexos expandem nosso sistema numérico para incluir as raízes quadradas de números negativos, onde a unidade 'imaginária' i é igual à raiz quadrada de -1. Programadores os utilizam em campos como engenharia, física e processamento de sinais, porque são excelentes para modelar ondas, oscilações e tudo que gira.

## Como fazer:

Vamos definir uma classe básica de número complexo em Kotlin:

```kotlin
data class Complex(val real: Double, val imaginario: Double) {
    operator fun plus(other: Complex) = Complex(real + other.real, imaginario + other.imaginario)
    operator fun minus(other: Complex) = Complex(real - other.real, imaginario - other.imaginario)
    operator fun times(other: Complex) = Complex(
        real * other.real - imaginario * other.imaginario,
        real * other.imaginario + imaginario * other.real
    )
    
    override fun toString(): String = "($real + ${imaginario}i)"
}

fun main() {
    val a = Complex(1.0, 2.0)
    val b = Complex(3.0, 4.0)
    
    println("a + b = ${a + b}")  // Saída: a + b = (4.0 + 6.0i)
    println("a - b = ${a - b}")  // Saída: a - b = (-2.0 - 2.0i)
    println("a * b = ${a * b}")  // Saída: a * b = (-5.0 + 10.0i)
}
```

## Aprofundamento

Números complexos foram mencionados pela primeira vez no século 16, resolvendo equações cúbicas que não possuíam soluções reais. Engenharia e física se beneficiam enormemente de números complexos para analisar circuitos de corrente alternada e formas de onda. Alternativamente, você poderia usar uma biblioteca como `koma` ou `ejml` da Kotlin para trabalhos mais pesados.

Operações em números complexos espelham os números reais, mas com atenção à unidade imaginária. A multiplicação, por exemplo, segue a propriedade distributiva, lembrando que `i^2 = -1`. Esta unidade imaginária nos permite representar números multidimensionais, cruciais em vários cálculos científicos.

## Veja Também

Bibliotecas de Matemática Kotlin:

- [koma](https://koma.kyonifer.com/): Uma biblioteca de computação científica para Kotlin.

Leitura Adicional sobre Números Complexos:

- [Wikipedia: Números Complexos](https://pt.wikipedia.org/wiki/N%C3%BAmero_complexo)
