---
title:                "Convertendo uma data em uma string"
html_title:           "C++: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Converter uma data para uma string significa mudar o formato de uma data (por exemplo, dia, mês e ano) para um texto simples. Os programadores fazem isso para facilitar a leitura, escrita e a operação com datas.

## Como Fazer:

Aqui está um exemplo de como converter uma data para uma string no Arduino atual.

```Arduino
#include <RTClib.h>

RTC_DS1307 rtc;

void setup () {
  Serial.begin(57600);
  rtc.begin();
}

void loop () {
  DateTime now = rtc.now();

  String data = String(now.day()) + "/" 
              + String(now.month()) + "/" 
              + String(now.year());

  Serial.println(data);
}
```
Neste código, `rtc.now()` devolve a data e hora atual. Queremos extrair o dia, mês e ano e convertê-los em strings. A saída deve ser algo parecido com isso:

```Arduino
23/9/2021
```
## Conhecendo Mais 

Converter datas para strings é uma prática comum da programação desde os tempos antigos, quando os sistemas eram limitados e a memória era valiosa. 

Existem alternativas para a biblioteca RTClib. Uma delas é a TimeLib, que também fornece funções para lidade com datas e horas.

Cada biblioteca tem suas próprias peculiaridades. No exemplo acima, a RTClib retorna o dia e o mês como um único dígito para datas de 1 a 9, em vez de adicionar um '0' à frente.

## Veja Também

- Documentação do RTClib: https://github.com/adafruit/RTClib
- Documentação do TimeLib: https://www.pjrc.com/teensy/td_libs_Time.html
- Tutorial para manipulação de data e hora no Arduino: https://learn.adafruit.com/ds1307-real-time-clock-breakout-board-kit/overview