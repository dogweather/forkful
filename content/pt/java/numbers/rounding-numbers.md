---
date: 2024-01-26 03:45:34.935448-07:00
description: "Arredondar n\xFAmeros significa ajust\xE1-los a um determinado grau\
  \ de precis\xE3o. Programadores fazem isso para simplificar n\xFAmeros para legibilidade,\
  \ cumprir\u2026"
lastmod: '2024-02-25T18:49:44.072083-07:00'
model: gpt-4-0125-preview
summary: "Arredondar n\xFAmeros significa ajust\xE1-los a um determinado grau de precis\xE3\
  o. Programadores fazem isso para simplificar n\xFAmeros para legibilidade, cumprir\u2026"
title: "Arredondamento de n\xFAmeros"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Arredondar números significa ajustá-los a um determinado grau de precisão. Programadores fazem isso para simplificar números para legibilidade, cumprir certas especificações, ou garantir que os cálculos se encaixem dentro de certos limites, como evitar erros de precisão na aritmética de ponto flutuante.

## Como fazer:
Java oferece várias maneiras de arredondar números. Aqui está uma demonstração rápida com `Math.round()`, `BigDecimal` e `DecimalFormat`.

```java
public class RoundingDemo {
    public static void main(String[] args) {
        double num = 123.4567;

        // Usando Math.round()
        long roundedNum = Math.round(num);
        System.out.println(roundedNum); // Saída: 123

        // Usando BigDecimal para mais controle
        BigDecimal bd = new BigDecimal(num).setScale(2, RoundingMode.HALF_UP);
        double roundedBigDecimal = bd.doubleValue();
        System.out.println(roundedBigDecimal); // Saída: 123.46

        // Usando DecimalFormat
        DecimalFormat df = new DecimalFormat("#.##");
        String formattedNum = df.format(num);
        System.out.println(formattedNum); // Saída: 123.46
    }
}
```

## Aprofundamento
Historicamente, arredondar números tem sido essencial para cálculos analógicos e foi transferido para a computação digital por eficiência e precisão. Erros de arredondamento, como aqueles da aritmética de ponto flutuante, demonstram que isso não é uma questão trivial — eles podem acumular e bagunçar cálculos, digamos, em aplicações aeroespaciais e financeiras.

Além de `Math.round()`, você tem o `BigDecimal`, que lhe dá um controle mais fino sobre a escala e o modo de arredondamento, e `DecimalFormat` para quando você precisa arredondar números como parte da formatação da saída de texto. Alternativas ao arredondamento incluem operações de piso, teto e truncamento, que são diferentes maneiras de lidar com precisão e são normalmente tratadas por vários métodos `Math`.

Dependendo do seu caso de uso, a estratégia de arredondamento pode variar. Por exemplo, `BigDecimal` é a escolha para cálculos financeiros, onde a precisão é crítica. Em contraste, `Math.round()` é uma maneira rápida para operações de propósito geral quando você é menos exigente sobre o modo de arredondamento.

## Veja Também
- [Documentação de Math da Java da Oracle](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/Math.html)
- [Padrão IEEE para Aritmética de Ponto Flutuante (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)
- [Classe DecimalFormat em Java](https://docs.oracle.com/javase/7/docs/api/java/text/DecimalFormat.html)
