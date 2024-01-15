---
title:                "Escrevendo testes"
html_title:           "Arduino: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes no Arduino?

Escrever testes é uma prática importante em qualquer tipo de programação, incluindo a programação do Arduino. Ao escrever testes, você pode garantir que seu código está funcionando corretamente e detectar possíveis erros antes mesmo de carregá-lo no seu Arduino. Além disso, escrever testes pode ajudar a entender melhor o funcionamento do seu código e a identificar áreas que podem ser melhoradas.

## Como escrever testes no Arduino

Para escrever testes no Arduino, você precisa seguir alguns passos simples. Primeiramente, é necessário definir quais são os objetivos dos seus testes e quais partes do seu código serão cobertas por eles. Em seguida, você deve criar funções de teste que verifiquem se o seu código está funcionando corretamente. Por último, é importante executar esses testes e analisar os resultados.

Um exemplo de teste no Arduino pode ser visto abaixo:

```Arduino
int pin = 13;

void setup() {
  pinMode(pin, OUTPUT);
}

void loop() {
  digitalWrite(pin, HIGH);
  delay(1000);
  digitalWrite(pin, LOW);
  delay(1000);
}

void testPinState() {
  digitalWrite(pin, HIGH);
  assert(digitalRead(pin) == HIGH);
  delay(1000);
  digitalWrite(pin, LOW);
  assert(digitalRead(pin) == LOW);
}
```

Neste exemplo, criamos uma função de teste que verifica se o pino 13 está no estado correto. Primeiro, definimos o pino como saída no setup e, em seguida, usamos a função `digitalWrite()` para alterar o estado do pino. Em seguida, usamos a função `digitalRead()` para verificar se o estado do pino corresponde ao que esperamos e usamos a função `assert()` para garantir que o teste falhará caso contrário. Por fim, executamos o código no loop e chamamos a função de teste para verificar se está funcionando corretamente.

## Aprofundando-se nos testes no Arduino

Uma forma mais avançada de escrever testes no Arduino é usar uma biblioteca de testes, como a [Arduino Unit Testing Library](https://github.com/mmurdoch/arduinounit), que automatiza parte do processo e oferece mais recursos, como a possibilidade de testar múltiplos códigos em um único teste. Além disso, é importante lembrar de manter seus testes atualizados à medida que seu código é alterado e adicionar novos testes sempre que novas funcionalidades forem adicionadas.

## Veja também

- [Tutorial de testes com Arduino](https://www.arduino.cc/en/Guide/UnitTesting)
- [Tutorial de testes com Arduino Unit Testing Library](https://www.youtube.com/watch?v=dNfZEXLcMgw)
- [Documentação oficial da Arduino Unit Testing Library](https://github.com/mmurdoch/arduinounit/wiki)