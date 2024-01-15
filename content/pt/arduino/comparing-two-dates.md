---
title:                "Comparando duas datas"
html_title:           "Arduino: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que

Comparar datas é uma tarefa comum na programação e pode ser útil para verificar a ocorrência de eventos ou para realizar cálculos baseados na diferença entre duas datas. Neste artigo, vamos aprender como comparar duas datas usando o Arduino.

## Como fazer

Para comparar duas datas no Arduino, primeiro precisamos converter as datas em um formato adequado. Podemos usar a biblioteca RTClib para trabalhar com datas e horários no Arduino.

Comece criando duas variáveis do tipo DateTime e atribua a elas as datas que deseja comparar. Por exemplo:

```Arduino
#include <RTClib.h>

RTC_DS1307 rtc; // cria um objeto rtc
DateTime data1(2019, 07, 15, 12, 30, 0); // primeira data a ser comparada
DateTime data2(2019, 07, 20, 9, 00, 0); // segunda data a ser comparada
```

Em seguida, podemos usar os métodos de comparação da biblioteca RTClib para verificar se as datas são iguais, maior ou menor que a outra. Por exemplo:

```Arduino
if(data1 == data2){ // verifica se as datas são iguais
  Serial.println("As datas são iguais!");
} else if(data1 > data2){ // verifica se a primeira data é maior que a segunda
  Serial.println("A data 1 é maior que a data 2!");
} else { // caso contrário, a primeira data é menor que a segunda
  Serial.println("A data 1 é menor que a data 2!");
}
```

Além disso, também podemos usar o método `deltaDays()` para calcular a diferença em dias entre as datas:

```Arduino
int diferenca = data2.deltaDays(data1); // calcula a diferença em dias entre data2 e data1
Serial.print("A diferença em dias entre as datas é: ");
Serial.println(diferenca); // imprime a diferença em dias no monitor serial
```

## Aprofundando

Para comparar datas com mais precisão, é importante entender o formato da variável DateTime no Arduino. O formato usado é `DateTime(ano, mês, dia, hora, minuto, segundo)`, onde todos os valores são inteiros.
Além disso, é importante ter em mente que a biblioteca RTClib considera datas no formato UTC (Tempo Universal Coordenado). Portanto, se você precisa comparar datas em um fuso horário específico, é necessário fazer ajustes nos valores de hora da data antes da comparação.

## Veja também

- [Documentação da biblioteca RTClib](https://github.com/adafruit/RTClib)
- [Guia completo para trabalhar com datas e horários no Arduino](https://www.tutorialspoint.com/arduino/arduino_date_time.htm)
- [Tutorial sobre como criar um relógio com data e hora no Arduino](https://www.instructables.com/id/Arduino-Real-Time-Clock-Using-DS1307/)