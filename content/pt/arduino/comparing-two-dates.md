---
title:                "Comparando duas datas"
html_title:           "C#: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O Que e Porque?

Comparar duas datas é o processo de determinar qual data é mais recente ou anterior em relação à outra. Programadores fazem isso para manipular dados relacionados ao tempo, para funções como agendamento de tarefas, temporizadores e medição do tempo decorrido.

## Como Fazer:

A plataforma Arduino não tem uma biblioteca embutida para lidar com datas, nós vamos usar a biblioteca `TimeLib`. Primeiro, há necessidade de instalá-la, vá para "Sketch >> Include Library >> Manage Libraries… >> busque por 'TimeLib'". Aqui está um exemplo de código de como comparar duas datas:

```Arduino
#include <TimeLib.h>

time_t primeiraData;
time_t segundaData;

void setup() {
  Serial.begin(9600); 
  
  primeiraData = makeTime(0, 0, 0, 15, 3, 2023); 
  segundaData = makeTime(0, 0, 0, 20, 3, 2023); 
}

void loop() {
  if(primeiraData < segundaData) {
    Serial.println("A primeira data é mais cedo!");
  } 
  else if(primeiraData > segundaData) {
    Serial.println("A primeira data é mais tarde!"); 
  }
  else {
    Serial.println("As datas são iguais!"); 
  }
  delay(2000);
}
```

## Um Mergulho Profundo:

Apesar do Arduino não ter suporte nativo para datas, a comunidade de programadores desenvolveu bibliotecas como a TimeLib que simplificam o trabalho com datas. Existem alternativas à TimeLib, como a RTClib, que oferece funções semelhantes e suporta relógios de tempo real (RTC). Em termos de implementação, a comparação de datas é feita convertendo as datas em segundos desde 1 de Janeiro de 1970 (um formato conhecido como "UNIX timestamp") e depois comparando esses valores.

## Ver Também:

- Documentação TimeLib: https://github.com/PaulStoffregen/Time
- RTClib por Adafruit: https://github.com/adafruit/RTClib
- Guia geral para datas e tempo no Arduino: https://www.makerguides.com/arduino-time-date-guide/