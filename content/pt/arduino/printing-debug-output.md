---
title:                "Arduino: Imprimindo saída de depuração"
programming_language: "Arduino"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

# Por que imprimir saída de depuração em Arduino?

Ao programar em Arduino, é comum encontrar erros e problemas no código. Nesses casos, a impressão de saída de depuração é extremamente útil, pois permite que você veja o que está acontecendo no seu programa e identifique possíveis erros mais facilmente.

# Como fazer:

Para imprimir saída de depuração em Arduino, você pode usar a função `Serial.print()` ou `Serial.println()`, que enviam dados para a porta serial do seu Arduino. Você pode usar essas funções para imprimir valores de variáveis, mensagens de texto ou qualquer outra informação que seja relevante para o seu programa.

Um exemplo simples de como usar a função `Serial.print()` para imprimir o valor de uma variável seria o seguinte:

```
Arduino

int sensorValue = 10;

Serial.print("O valor do sensor é: ");
Serial.println(sensorValue);
```

Neste exemplo, a primeira linha declara a variável `sensorValue` com o valor de 10. Na segunda linha, usamos a função `Serial.print()` para imprimir a mensagem "O valor do sensor é: " sem pular uma linha. Depois, usamos a função `Serial.println()` para imprimir o valor da variável `sensorValue` e pular uma linha.

A saída deste código seria:

```
O valor do sensor é: 10
```

Você também pode combinar ambas as funções para imprimir uma mensagem e o valor de uma variável na mesma linha, como mostrado no exemplo abaixo:

```
Arduino

int temperatura = 25;

Serial.print("A temperatura atual é: ");
Serial.println(temperatura);
```

A saída deste código seria:

```
A temperatura atual é: 25
```

# Aprofundando:

Além de imprimir valores, você também pode usar a função `Serial.print()` ou `Serial.println()` para imprimir mensagens de texto para ajudar no processo de depuração. Por exemplo:

```
Arduino

Serial.println("Este é um exemplo de mensagem de depuração.");
```

A saída deste código seria:

```
Este é um exemplo de mensagem de depuração.
```

Lembre-se também de que, ao usar a função `Serial.print()` ou `Serial.println()`, é importante ter um monitor serial conectado ao seu Arduino para que você possa ver a saída. Você pode abrir o monitor serial clicando em "Ferramentas" e depois em "Monitor Serial" no software da IDE do Arduino.

# Veja também:

- [Documentação oficial do Arduino sobre impressão de saída de depuração](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- [Vídeo tutorial sobre como usar o monitor serial do Arduino](https://www.youtube.com/watch?v=qRzcg4w2tnc)
- [Artigo sobre depuração de código em Arduino](https://create.arduino.cc/projecthub/ianl10/debugging-arduino-code-the-easy-way-45b35e)