---
title:                "Geração de números aleatórios"
html_title:           "Arduino: Geração de números aleatórios"
simple_title:         "Geração de números aleatórios"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

##Por que gerar números aleatórios em Arduino?

Gerar números aleatórios pode ser útil em várias aplicações, como jogos, sorteios ou até mesmo em projetos de segurança. Além disso, pode adicionar um elemento imprevisível e divertido em suas criações com Arduino.

##Como fazer:

 Para gerar números aleatórios em Arduino, você precisará usar a função `random()`. É importante mencionar que esta função gera números pseudo-aleatórios, ou seja, eles não são totalmente aleatórios, mas podem ser suficientemente aleatórios para a maioria das aplicações.

Veja um exemplo de código usando a função `random()` para gerar um número aleatório entre 1 e 10 e imprimi-lo no monitor serial:

```
Arduino int num = random(1, 11); //gera um número aleatório entre 1 e 10
Serial.println(num); //imprime o número gerado no monitor serial
```

E a saída seria algo como:

```
9
```

Você também pode gerar números aleatórios dentro de um loop, para criar uma sequência de números aleatórios:

```
int num;

void setup() {
  Serial.begin(9600);
}

void loop() {

  for(int i=0; i<5; i++) { //gera 5 números aleatórios
    num = random(50); //gera um número aleatório entre 0 e 49
    Serial.println(num); //imprime o número gerado no monitor serial
    delay(500); //aguarda 500 milissegundos
  }
  
}
```

E a saída seria algo como:

```
27
9
34
19
40
```

##Aprofundando mais:

Se você quiser aprender mais sobre como a função `random()` funciona, vale a pena dar uma olhada na documentação oficial do Arduino. Você também pode explorar outras funções relacionadas, como `randomSeed()` e `randomGaussian()`, que podem ajudar a gerar números aleatórios mais complexos.

Além disso, é importante ter em mente que a função `random()` usa um algoritmo para gerar os números, o que pode resultar em alguns padrões se você usá-la repetidamente sem definir um valor inicial (seed). É por isso que a função `randomSeed()` é útil, pois define um valor inicial para o algoritmo.

##Veja também:

- [Documentação oficial do Arduino sobre a função `random()`](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Artigo sobre geração de números aleatórios em Arduino](https://www.arduino.cc/en/Tutorial/BuiltInExamples/RandomNumbers)
- [Função `random()` em ação em um projeto de dado eletrônico](https://create.arduino.cc/projecthub/Arduino_Genuino/digital-dice-eb9010?ref=tag&ref_id=random&offset=5)