---
title:                "Java: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que utilizar a geração de números aleatórios em Java?

A geração de números aleatórios é uma técnica muito útil na programação em Java. Ela permite que o programa gere valores aleatórios, ou seja, que não seguem uma sequência lógica definida, para serem utilizados em diversas situações. Esses números podem ser utilizados em jogos, sorteios, criptografia, entre outras aplicações. Além disso, a geração de números aleatórios pode ser uma forma de aumentar a eficiência e segurança de um programa.

## Como gerar números aleatórios em Java?

Para gerar números aleatórios em Java, existem algumas classes e métodos que podem ser utilizados. Vamos ver abaixo um exemplo de código que gera um número inteiro aleatório entre 1 e 10:

```Java
import java.util.Random;

public class GeraAleatorio {
  public static void main(String[] args) {
    // Cria um objeto da classe Random
    Random gerador = new Random();
    
    // Gera um número aleatório entre 1 e 10
    int num = gerador.nextInt(10) + 1;
    
    // Imprime o número gerado
    System.out.println("O número aleatório gerado é: " + num);
  }
}
```

O resultado da execução deste código pode ser algo como:

```
O número aleatório gerado é: 7
```

Além do método `nextInt()`, a classe `Random` também possui outros métodos para gerar números aleatórios, como `nextDouble()` e `nextBoolean()`. É importante lembrar que esses números não são verdadeiramente aleatórios, mas sim pseudoaleatórios, ou seja, eles seguem uma sequência determinística que é difícil de prever. Para entender melhor o funcionamento desses métodos e como eles geram os números, vamos dar uma olhada na seção "Detalhes da Geração de Números Aleatórios".

## Detalhes da Geração de Números Aleatórios

A classe `Random` utiliza um algoritmo para gerar seus números aleatórios, e esse algoritmo pode variar de acordo com a versão do Java que está sendo utilizada. Antes da versão 8, o algoritmo padrão era o *linear congruential generators*, mas a partir do Java 8, o algoritmo utilizado é o *xorshift generators*. Esses algoritmos são baseados em operações matemáticas e de bit shifting que garantem uma sequência pseudoaleatória.

Outro ponto importante é que a classe `Random` possui uma *seed*, que é um valor inicial para gerar os números. Se nenhum valor for especificado, ela utiliza o tempo atual como *seed*, mas é possível definir uma *seed* personalizada através do construtor `Random(seed)`. Isso permite que um mesmo conjunto de números seja gerado sempre que a mesma *seed* for utilizada.

## Veja também

- [Documentação oficial da classe Random em Java](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Tutorial do Java Random: Como gerar números aleatórios em Java](https://www.baeldung.com/java-random)
- [Vídeo sobre geração de números aleatórios em Java](https://www.youtube.com/watch?v=s-yJy5MqYPg)