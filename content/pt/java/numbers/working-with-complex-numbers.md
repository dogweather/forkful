---
date: 2024-01-26 04:41:55.018579-07:00
description: "Como Fazer: Java n\xE3o tem suporte embutido para n\xFAmeros complexos,\
  \ mas podemos criar nossa pr\xF3pria classe ou usar uma biblioteca. Aqui est\xE1\
  \ um exemplo\u2026"
lastmod: '2024-03-13T22:44:46.450113-06:00'
model: gpt-4-0125-preview
summary: "Java n\xE3o tem suporte embutido para n\xFAmeros complexos, mas podemos\
  \ criar nossa pr\xF3pria classe ou usar uma biblioteca."
title: "Trabalhando com n\xFAmeros complexos"
weight: 14
---

## Como Fazer:
Java não tem suporte embutido para números complexos, mas podemos criar nossa própria classe ou usar uma biblioteca. Aqui está um exemplo rápido de como criar uma classe `ComplexNumber` simples e usá-la:

```java
public class ComplexNumber {
    private double real;
    private double imaginario;

    public ComplexNumber(double real, double imaginario) {
        this.real = real;
        this.imaginario = imaginario;
    }

    public ComplexNumber add(ComplexNumber other) {
        return new ComplexNumber(this.real + other.real, this.imaginario + other.imaginario);
    }

    // ToString para exibir números complexos no formato a + bi
    @Override
    public String toString() {
        return String.format("%.1f + %.1fi", real, imaginario);
    }

    // Teste rápido
    public static void main(String[] args) {
        ComplexNumber c1 = new ComplexNumber(2, 3);
        ComplexNumber c2 = new ComplexNumber(1, 4);

        System.out.println("Soma: " + c1.add(c2));
    }
}
```

A saída de amostra para o método principal será:

```
Soma: 3.0 + 7.0i
```

## Aprofundamento
Antes de linguagens de alto nível como Java, programadores trabalhavam diretamente com bibliotecas matemáticas em linguagens como Fortran ou C para gerenciar operações complexas. O conceito remonta ao século 16, creditado a matemáticos como Gerolamo Cardano e Rafael Bombelli.

Em Java, `java.lang.Math` é o recurso para o essencial, mas omite números complexos, provavelmente porque nem todo programador os usa. Alternativas? Use bibliotecas. O Apache Commons Math fornece uma classe `Complex` repleta de métodos para manipulação. Aqui está o motivo pelo qual criar a sua própria classe é interessante: Leveza, sob medida para suas necessidades exatas e sem sobrecarga de biblioteca.

Um detalhe importante: cuidado com a precisão de ponto flutuante. Computadores não conseguem representar alguns números exatamente, levando a erros de arredondamento. Ao realizar operações complexas repetitivas, esses erros podem se acumular!

## Veja Também
Para aprofundamentos e operações mais complexas, confira:

- [Apache Commons Math](https://commons.apache.org/proper/commons-math/)
- [Classe Complex do JScience](http://jscience.org/)
- Tutoriais da Oracle sobre [aritmética de ponto flutuante](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
