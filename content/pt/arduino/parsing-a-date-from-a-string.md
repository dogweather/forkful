---
title:                "Analisando uma data a partir de uma string"
date:                  2024-01-20T15:34:35.077479-07:00
html_title:           "Arduino: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?
Extrair uma data de uma string significa decifrar uma data inserida como texto. Programadores fazem isso para transformar a informação em formatos úteis para armazenar, comparar ou manipular datas.

## Como Fazer:
Examinando a data em uma string formatada como "DD/MM/AAAA", você pode decompor e converter os componentes dia, mês e ano para inteiros.

```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS1307 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("RTC não encontrado !");
    while (1);
  }

  String dataString = "31/12/2023";
  int dia = dataString.substring(0, 2).toInt();
  int mes = dataString.substring(3, 5).toInt();
  int ano = dataString.substring(6).toInt();

  rtc.adjust(DateTime(ano, mes, dia));

  DateTime agora = rtc.now();
  Serial.print("Data ajustada para: ");
  Serial.print(agora.day());
  Serial.print("/");
  Serial.print(agora.month());
  Serial.print("/");
  Serial.println(agora.year());
}

void loop() {
  // Nada a fazer aqui no loop.
}
```
Saída esperada:
```
Data ajustada para: 31/12/2023
```
## Mergulho Profundo
Antes das bibliotecas especializadas, a conversão entre strings e datas exigia manipulação manual da string. Alternativas modernas incluem o uso de bibliotecas como `RTClib`, que facilitam a integração com módulos de tempo real (RTC). A implementação depende da consistência do formato da string. Data e hora são vitais para registrar eventos, marcar timestamps, e agendar tarefas em sistemas embarcados.

## Veja Também
- Documentação da biblioteca RTClib (https://github.com/adafruit/RTClib)
- Referência sobre a classe String na Arduino (https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- Informações sobre módulos RTC e Arduino (https://www.arduino.cc/en/Guide/Libraries#toc4)
