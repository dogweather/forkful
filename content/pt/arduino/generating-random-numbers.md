---
title:    "Arduino: Gerando números aleatórios"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios no Arduino?

Gerar números aleatórios é uma necessidade comum em muitos projetos de Arduino. Esses números podem ser usados em jogos, simulações, desafios matemáticos e muito mais. Além disso, eles também são úteis para testar códigos e criar resultados imprevisíveis.

## Como fazer isso:

Gerar números aleatórios no Arduino é bastante simples. A biblioteca padrão do Arduino já possui uma função chamada `random()`, que retorna um número inteiro aleatório. Porém, para ter certeza de que os números gerados são realmente aleatórios, é importante seguir alguns passos adicionais.

Primeiro, é necessário usar a função `randomSeed()` para inicializar a semente dos números aleatórios. Isso pode ser feito usando um valor variável, como a leitura de um pino analógico.

Em seguida, é possível especificar um intervalo para os números aleatórios usando a função `random(min, max)`. Por exemplo, se quiser gerar um número aleatório entre 1 e 10, basta usar `random(1, 10)`.

A seguir, um exemplo de código para gerar três números aleatórios entre 1 e 100 e exibi-los no monitor serial:

```Arduino
int num1, num2, num3;
void setup(){
  Serial.begin(9600);
  randomSeed(analogRead(A0));
}

void loop(){
  num1 = random(1, 100);
  num2 = random(1, 100);
  num3 = random(1, 100);

  Serial.print("Números aleatórios: ");
  Serial.print(num1);
  Serial.print(", ");
  Serial.print(num2);
  Serial.print(", ");
  Serial.println(num3);
  delay(5000);
}
```

Eis um possível resultado da saída no monitor serial:

```
Números aleatórios: 65, 24, 98
```

## Profundidade:

Agora que sabemos como gerar números aleatórios no Arduino, é importante entender como isso funciona por trás dos bastidores. A função `random()` usa um algoritmo matemático para gerar os números, mas esses algoritmos têm limitações e podem não gerar um resultado verdadeiramente aleatório.

Uma maneira de melhorar o nível de aleatoriedade é usar uma fonte externa de ruído, como um sensor de luz ou um microfone, para alimentar a semente dos números aleatórios. Isso torna os números gerados mais imprevisíveis e aleatórios.

Além disso, é importante lembrar que os números gerados pelo Arduino não são realmente aleatórios - eles são pseudoaleatórios, pois seguem um padrão matemático. Portanto, é sempre bom usar vários métodos de aleatoriedade para garantir resultados mais precisos.

## Veja também:

- Documentação da função `random()` do Arduino: https://www.arduino.cc/reference/pt/language/functions/random numbers/
- Tutorial sobre geração de números aleatórios no Arduino: https://blog.arduino.cc/2017/09/20/how-to-generate-random-numbers-with-the-arduino-using-the-random-library/
- Vídeo explicando os conceitos por trás da criação de números aleatórios: https://www.youtube.com/watch?v=9L-6QaUJSHQ