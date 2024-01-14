---
title:                "Arduino: Gerando números aleatórios"
programming_language: "Arduino"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios é importante para o seu projeto Arduino

Gerar números aleatórios é crucial em muitos projetos que envolvem a placa Arduino. Eles podem ser usados para criar padrões imprevisíveis ou para simular condições aleatórias em jogos e simulações. Além disso, a geração de números aleatórios também é útil em projetos de segurança, como sistemas de bloqueio ou autenticação.

## Como gerar números aleatórios em seu código Arduino

Para gerar números aleatórios em seu código Arduino, você precisará utilizar a função "random()". Esta função aceita dois parâmetros, o primeiro sendo o valor mínimo desejado e o segundo sendo o valor máximo desejado. Por exemplo, se você quiser gerar um número aleatório entre 0 e 10, seu código ficaria assim:

```
Arduino
int numeroAleatorio = random(0, 10);
```

Você também pode gerar números aleatórios dentro de um loop, para obter uma série de valores aleatórios ao longo do tempo. Basta declarar a função "random()" dentro do seu loop e adicionar uma pausa para que os números não sejam gerados rapidamente demais.

```
Arduino
void loop(){
  int numeroAleatorio = random(0, 10);
  delay(100); //pausa de 100 milissegundos
}
```

## Mergulhando mais fundo na geração de números aleatórios no Arduino

A função "random()" não é realmente aleatória, pois ela utiliza um algoritmo matemático para gerar os números. No entanto, você pode torná-la mais aleatória ao usar um valor "semente" (seed) como parâmetro. Isso pode ser um valor fixo, como o tempo do sistema, ou um valor que varia, como a leitura de uma porta analógica.

```
Arduino
//uso de um valor fixo como "semente"
int numeroAleatorio = random(0, 10, 123);

//uso de uma leitura da porta analógica como "semente"
int semente = analogRead(A0);
int numeroAleatorio = random(0, 10, semente);
```

Além disso, se você quiser gerar números aleatórios dentro de um intervalo específico, você pode utilizar a função "randomSeed()" para alterar o valor inicial do algoritmo. Isso pode ser feito em qualquer ponto do seu código.

```
Arduino
randomSeed(42); //altera o valor inicial do algoritmo
int numeroAleatorio = random(0, 10);
```

## Veja também

- [Tutorial de geração de números aleatórios no Arduino (em inglês)](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Artigo sobre o uso de valores semente na geração de números aleatórios no Arduino (em inglês)](https://www.arduino.cc/reference/en/language/functions/random-numbers/randomseed/)
- [Exemplo de projeto Arduino que utiliza geração de números aleatórios (em inglês)](https://create.arduino.cc/projecthub/ibnashahab/random-number-generator-using-arduino-293b5c)