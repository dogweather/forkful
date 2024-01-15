---
title:                "Obtendo a data atual"
html_title:           "Arduino: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que?

Geralmente, em projetos que envolvem sistemas automatizados e dispositivos eletrônicos, é importante ter a data e hora atual para fins de registro, programação de tarefas e outras aplicações que dependem do tempo.

## Como fazer

```
#include <Time.h>
#include <TimeLib.h>

void setup() {
  Serial.begin(9600); // Inicializa a conexão serial
  setTime(19, 45, 00, 15, 9, 2021); // Define a data e hora atual
}

void loop() {
  // Imprime a data no formato DD/MM/AAAA
  Serial.println(day() + "/" + month() + "/" + year());
  // Imprime a hora no formato HH:MM:SS
  Serial.println(hour() + ":" + minute() + ":" + second());
  delay(1000); // Aguarda 1 segundo
}
```

O trecho de código acima utiliza as bibliotecas Time.h e TimeLib.h para definir a data e hora atual e imprimi-las no monitor serial. É importante notar que a função setTime() deve ser chamada na função setup() antes de utilizarmos as funções relacionadas à data e hora.

Além disso, existe a possibilidade de obter a data e hora atual através de um módulo RTC (Real Time Clock), que consiste em um circuito integrado com um relógio e uma bateria de backup que mantém a contagem do tempo mesmo sem energia. Nesse caso, é necessário utilizar uma biblioteca específica para o módulo utilizado.

## Deep Dive

O Arduino possui um timer interno de 16 bits que é atualizado a cada microssegundo. Esse timer é utilizado pelas bibliotecas de data e hora para contar o tempo desde uma data de referência (Unix Epoch - 1º de janeiro de 1970). A partir dessa contagem, é possível converter para os formatos de data e hora que estamos acostumados a utilizar.

Além disso, é importante mencionar que o Arduino possui um relógio interno que mantém a data e hora mesmo quando desligado, mas essa informação só é precisa se o microcontrolador estiver conectado à alimentação. Caso contrário, a data e hora serão resetadas na próxima vez que o Arduino for ligado.

## Veja também

- [Biblioteca TimeLib](https://github.com/PaulStoffregen/Time)
- [Biblioteca DS3231](https://github.com/NorthernWidget/DS3231) (para utilização com módulo RTC)
- [Tutorial sobre como usar a biblioteca Time](https://learn.adafruit.com/adafruit-arduino-lesson-6-digital-inputs/using-a-real-time-clock)