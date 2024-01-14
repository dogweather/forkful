---
title:    "Arduino: Escrevendo testes"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Por que escrever testes em Arduino?

Escrever testes em código Arduino pode parecer uma tarefa desnecessária, mas é extremamente importante para garantir que o seu projeto funcione corretamente. Testes garantem que seus componentes eletrônicos estão funcionando corretamente e evitam erros e falhas no seu código.

## Como escrever testes em Arduino

Para escrever testes em Arduino, você precisa utilizar o Arduino IDE e alguns componentes eletrônicos, como LEDs e resistores. Primeiro, você deve definir uma função de teste que irá verificar se o componente está funcionando corretamente. Por exemplo, se você quiser testar um LED, sua função de teste pode acender o LED e verificar se está emitindo luz. Veja um exemplo abaixo usando o Arduino UNO e um LED:

```Arduino
const int ledPin = 13;

void setup() {
  pinMode(ledPin, OUTPUT);
}

void loop() {
  digitalWrite(ledPin, HIGH); // liga o LED
  delay(1000); // espera 1 segundo
  digitalWrite(ledPin, LOW); // desliga o LED
  delay(1000); // espera 1 segundo
}
```

No exemplo acima, o LED será aceso por 1 segundo e depois desligado por 1 segundo. Se o LED estiver funcionando corretamente, você deve vê-lo piscando. Agora, você pode adicionar essa função de teste ao seu código principal e executá-la sempre que quiser verificar o funcionamento do LED.

## Aprofundando nos testes em Arduino

Além de testar componentes eletrônicos, você também pode escrever testes para verificar a lógica do seu código. Por exemplo, se seu projeto envolve diferentes condições para executar diferentes tarefas, você pode escrever testes para verificar se todas as condições são tratadas corretamente. Além disso, você também pode utilizar bibliotecas de teste em Arduino, como a "ArduinoUnit", para facilitar a escrita e execução de testes.

## Veja também

- [Tutorial de testes em Arduino](https://www.arduino.cc/en/Guide/Introduction)
- [Biblioteca ArduinoUnit](https://github.com/mmurdoch/arduinounit)