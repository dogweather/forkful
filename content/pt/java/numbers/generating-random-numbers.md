---
date: 2024-01-27 20:34:02.785467-07:00
description: "Como fazer: Em Java, gerar n\xFAmeros aleat\xF3rios pode ser alcan\xE7\
  ado utilizando a classe `Random` do pacote `java.util`, ou as classes `ThreadLocalRandom`\
  \ e\u2026"
lastmod: '2024-03-13T22:44:46.451978-06:00'
model: gpt-4-0125-preview
summary: "Em Java, gerar n\xFAmeros aleat\xF3rios pode ser alcan\xE7ado utilizando\
  \ a classe `Random` do pacote `java.util`, ou as classes `ThreadLocalRandom` e `SecureRandom`\
  \ para casos de uso espec\xEDficos."
title: "Gera\xE7\xE3o de n\xFAmeros aleat\xF3rios"
weight: 12
---

## Como fazer:
Em Java, gerar números aleatórios pode ser alcançado utilizando a classe `Random` do pacote `java.util`, ou as classes `ThreadLocalRandom` e `SecureRandom` para casos de uso específicos. Os exemplos a seguir ilustram como utilizar essas classes.

### Utilizando a classe `Random`
A classe `Random` oferece uma maneira de gerar números pseudoaleatórios simples.

```Java
import java.util.Random;

public class RandomExample {
    public static void main(String[] args) {
        Random rand = new Random(); // Cria um objeto Random

        int randInt = rand.nextInt(50); // Gera um inteiro aleatório de 0 a 49
        double randDouble = rand.nextDouble(); // Gera um double aleatório entre 0.0 e 1.0
        boolean randBoolean = rand.nextBoolean(); // Gera um boolean aleatório
        
        System.out.println("Int Aleatório: " + randInt);
        System.out.println("Double Aleatório: " + randDouble);
        System.out.println("Boolean Aleatório: " + randBoolean);
    }
}
```

### Utilizando a classe `ThreadLocalRandom`
Para aplicações concorrentes, `ThreadLocalRandom` é mais eficiente que `Random`.

```Java
import java.util.concurrent.ThreadLocalRandom;

public class ThreadLocalRandomExample {
    public static void main(String[] args) {
        int randInt = ThreadLocalRandom.current().nextInt(1, 101); // De 1 a 100
        double randDouble = ThreadLocalRandom.current().nextDouble(1.0, 10.0); // De 1.0 a 10.0
        
        System.out.println("Int Aleatório: " + randInt);
        System.out.println("Double Aleatório: " + randDouble);
    }
}
```

### Utilizando a classe `SecureRandom`
Para operações criptográficas, `SecureRandom` proporciona um nível mais alto de segurança.

```Java
import java.security.SecureRandom;

public class SecureRandomExample {
    public static void main(String[] args) {
        SecureRandom secRand = new SecureRandom();
        
        byte[] bytes = new byte[20];
        secRand.nextBytes(bytes); // Preenche bytes com números aleatórios seguros
        
        System.out.println("Bytes Aleatórios Seguros:");
        for (byte b : bytes) {
            System.out.printf("%02x ", b);
        }
    }
}
```

## Aprofundamento
A geração de números aleatórios evoluiu significativamente desde os primórdios da computação. A classe `Random` de Java utiliza uma fórmula congruencial linear para gerar números pseudoaleatórios, que são determinísticos e não adequados para aplicações de alta segurança. Isso levou à introdução de `SecureRandom`, que utiliza algoritmos mais sofisticados (por exemplo, SHA1PRNG) para produzir números aleatórios criptograficamente fortes.

Contudo, `Random` e `SecureRandom` têm suas limitações, como a degradação de desempenho em ambientes multithread. A classe `ThreadLocalRandom` foi introduzida no Java 7 para resolver esta questão, fornecendo geradores de números aleatórios locais a threads, melhorando significativamente o desempenho em aplicações concorrentes.

Embora estas classes atendam a maioria das necessidades, para requisitos extremamente elevados ou especializados, os desenvolvedores podem explorar bibliotecas adicionais ou desenvolver soluções personalizadas. É essencial escolher a abordagem correta com base nas necessidades de segurança e nos requisitos de desempenho do caso de uso.
