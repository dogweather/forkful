---
title:                "Arduino: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

##Por que converter uma data em uma string?

Ao utilizar o Arduino para criar projetos, muitas vezes é necessário exibir informações de data e hora em um display ou salvar em um arquivo. No entanto, as datas são normalmente armazenadas em formatos numéricos, o que pode ser confuso e não tão intuitivo para o usuário final. Ao converter uma data em uma string, tornamos essa informação mais legível e fácil de ser entendida.

##Como fazer:

Para converter uma data em uma string no Arduino, podemos usar a biblioteca "Time.h". Primeiro, precisamos inicializar a biblioteca e declarar as variáveis necessárias para armazenar a data e hora. Em seguida, usamos a função "now ()" para obter os valores atuais de data e hora e, por fim, formatamos esses valores em uma string usando a função "String ()". Aqui está um exemplo de código:

```Arduino
#include <Time.h>

TimeElements tm;
String dataString;

void setup() {
  //inicializa a comunicação serial
  Serial.begin(9600);
  
  //define a hora atual no formato HH-MM-SS-DD-MM-YY
  tm.Hour = 12;
  tm.Minute = 30;
  tm.Second = 0;
  tm.Day = 5;
  tm.Month = 3;
  tm.Year = 2021;
  
  //converte a data em uma string
  dataString = String(tm.Hour) + ":" + String(tm.Minute) + ":" + String(tm.Second) + " - " + String(tm.Day) + "/" + String(tm.Month) + "/" + String(tm.Year);
  
  //imprime a string no monitor serial
  Serial.println(dataString);
}

void loop() {
  //não é necessário fazer nada no loop()
}
```

O resultado no monitor serial seria:

>12:30:0 - 5/3/2021

##Mergulho profundo:

Além de usar a biblioteca "Time.h", também é possível converter uma data em uma string usando a biblioteca "RTClib". Essa biblioteca é especialmente útil para projetos que envolvem o uso de módulos de tempo real ou relógios de tempo real. Com essa biblioteca, podemos obter a data atual diretamente do módulo e formatá-la em uma string. Aqui está um exemplo de código:

```Arduino
#include "RTClib.h"

RTC_PCF8523 rtc; //inicializa o módulo RTC

String dataString;

void setup() {
  //inicializa a comunicação serial
  Serial.begin(9600);
  
  //inicializa o módulo RTC
  rtc.begin();
  
  //verifica se o módulo está funcionando corretamente
  if (! rtc.begin()) {
    Serial.println("Módulo RTC não encontrado!");
    while (1);
  }
  
  //define a hora atual no formato HH-MM-SS-DD-MM-YY
  rtc.adjust(DateTime(F(__TIME__), F(__DATE__)));
  
  //obtém a data atual do módulo e converte em string
  DateTime now = rtc.now();
  dataString = now.toString("HH:mm:ss - dd/MM/yyyy");
  
  //imprime a string no monitor serial
  Serial.println(dataString);
}

void loop() {
  //não é necessário fazer nada no loop()
}
```

O resultado no monitor serial seria:

>12:30:00 - 05/03/2021

##Veja também:

- Documentação da biblioteca "Time.h": https://www.arduino.cc/reference/en/libraries/time/
- Documentação da biblioteca "RTClib": https://learn.adafruit.com/ds1307-real-time-clock-breakout-board-kit/using-the-library
- Tutorial de como usar um módulo RTC com o Arduino: https://www.filipeflop.com/blog/utilizando-modulo-rtc-ds1307-com-arduino/
- Vídeo tutorial sobre como converter uma data em uma string com o Arduino: https://www.youtube.com/watch?v=95pXo0oJ8EM