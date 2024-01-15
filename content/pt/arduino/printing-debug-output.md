---
title:                "Imprimindo saída de depuração"
html_title:           "Arduino: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que

Imprimir saída de debug é uma prática essencial no desenvolvimento de projetos no Arduino. Isso permite que você visualize informações importantes durante a execução do código, facilitando o processo de identificação e correção de erros.

## Como imprimir saída de debug usando Arduino

Imprimir saída de debug no Arduino é simples e pode ser feito usando a função `Serial.print()`. Para isso, você precisa seguir os seguintes passos:

1. Conectar o Arduino ao computador através do cabo USB;
2. Abrir a IDE do Arduino;
3. Criar um novo sketch;
4. Incluir a biblioteca `Serial` no início do seu código (`#include <Serial.h>`);
5. Inicializar a comunicação serial na função `setup()` com o comando `Serial.begin()`;
6. Utilizar a função `Serial.print()` para imprimir a informação desejada;
7. Carregar o código para o Arduino e abrir o Monitor Serial.

Um exemplo de código seria:

```
```Arduino 
#include <Serial.h>

void setup() {
  Serial.begin(9600);
}

void loop() {
  int temperatura = 25;
  Serial.print("A temperatura atual é: ");
  Serial.print(temperatura);
  delay(1000);
}
``` 
```

Esse código irá imprimir a informação da temperatura na porta serial a cada segundo. Você pode alterar o valor da temperatura e observar a mudança na saída. 

## Aprofundando-se

Existem outras formas de imprimir saída de debug no Arduino, como o uso das funções `Serial.println()`, `Serial.write()` e `Serial.printf()`. Além disso, é possível utilizar a porta serial para enviar informações para o computador e vice-versa, permitindo a comunicação entre os dispositivos.

É importante lembrar que a comunicação serial tem uma velocidade limitada, portanto, é recomendado utilizar a função `Serial.print()` apenas para fins de debug. Para enviar dados no formato binário, é necessário utilizar outras técnicas, como a criação de uma estrutura de dados.

## Veja também

- [Documentação oficial do Arduino](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- [Tutorial sobre comunicação serial no Arduino](https://www.arduino.cc/en/Tutorial/SerialCommunication)
- [Vídeo tutorial sobre impressão de saída de debug no Arduino](https://www.youtube.com/watch?v=ihzkKoGKXtc)