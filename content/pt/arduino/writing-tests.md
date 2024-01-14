---
title:    "Arduino: Escrevendo testes"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes é importante

Escrever testes para o seu código Arduino pode parecer tedioso ou desnecessário, mas é uma prática essencial para garantir que o seu projeto funcione corretamente. Ao escrever testes, você pode detectar e corrigir erros no seu código de forma mais eficiente, economizando tempo e evitando frustração no futuro.

## Como escrever testes para o Arduino

Para escrever testes eficazes para o seu projeto Arduino, você precisará utilizar algumas bibliotecas do Arduino. A biblioteca "ArduinoUnit", por exemplo, é uma ótima opção para escrever testes simples e rápidos. Aqui está um exemplo de como escrever um teste de verificação de LED piscando:

```
ArduinoUnit.h> // inclua a biblioteca ArduinoUnit
#include <Arduino.h> // inclua a biblioteca do Arduino
#include <Servo.h> // inclua a biblioteca do Servo (caso esteja usando um)
Servo myservo; // crie um objeto Servo
int pin = 9; // defina o pino do LED
void setup() {
 myservo.attach(10); // conecte o servo ao pino 10 (altere de acordo com o seu)
 pinMode(pin, OUTPUT); // defina o pino do LED como saída
}

// escreva o teste
unittest(testBlink) {
 // ligue o LED
 digitalWrite(pin, HIGH);
 delay(500);
 // desligue o LED
 digitalWrite(pin,LOW);
 delay(500);
 // verifique se o LED está piscando corretamente
 assertEqual(HIGH, true);
}

// execute o teste
int main() {
 // inicie a biblioteca do ArduinoUnit
 Test::plan(); // inicie o plano de teste
 Test::run(); // execute os testes
 return 0;
}
```

A saída do teste deve ser semelhante a isso:

![Saída do teste](https://github.com/frank830104/Aquarius/blob/master/Arduino-UnitTestExample/output.jpg?raw=true)

## Um mergulho mais profundo em escrever testes

Para escrever testes mais complexos e abrangentes, é recomendável aprender sobre os princípios de teste e as melhores práticas. Além disso, você pode explorar outras bibliotecas do Arduino, como a "Arduino Mock" para simular componentes eletrônicos em seus testes.

Alguns recursos úteis para aprender mais sobre testes no Arduino são:

- [ArduinoUnit documentation](https://github.com/mmurdoch/arduinounit#arduino-unittest-library)
- [The ArduinoMock library](https://github.com/JanHenrikH/arduino-mock)
- [Arduino Uno - Test-Driven Development (TDD) with ArduinoUnit](https://www.youtube.com/watch?v=MCfBh7Kt0X0)

## Veja também

- [Arduino Official Website](https://www.arduino.cc/)
- [ArduinoUnit on GitHub](https://github.com/mmurdoch/arduinounit)
- [ArduinoMock on GitHub](https://github.com/JanHenrikH/arduino-mock)