---
title:                "Obtendo a data atual"
html_title:           "C: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Artigo de Programação Arduino: Obtendo a Data Atual

## O Que & Por quê?

Identificar a data atual significa ler a data e o horário calculados no momento exato da execução. Fazê-lo ajuda os programadores a criar os registros de eventos, realizar automações baseadas no tempo e inserir marcas temporais nos dados.

## Como Fazer:

Utilizando o modelo de relógio em tempo real DS3231, podemos obter a data e a hora atual com o Arduino. Aqui está um exemplo de como:
```Arduino
#include "RTClib.h"

RTC_DS3231 rtc;

void setup () {
  Serial.begin(9600);
  if (! rtc.begin()) {
    Serial.println("DS3231 RTC não encontrado!");
    while (1);
  }
}

void loop () {
  DateTime agora = rtc.now();
  Serial.print(agora.day(), DEC);
  Serial.print('/');
  Serial.print(agora.month(), DEC);
  Serial.print('/');
  Serial.print(agora.year(), DEC);
  Serial.print(' ');
  Serial.print(agora.hour(), DEC);
  Serial.print(':');
  Serial.print(agora.minute(), DEC);
  Serial.print(':');
  Serial.println(agora.second(), DEC);
  delay(1000);
}
```
A saída desse código seria algo como:
```Arduino
24/04/2023 14:25:15
```
Essa saída representa a data (dia/mês/ano) e a hora (hora:minuto:segundo) atual.

## Mergulho Profundo:

Ao longo dos anos, os arduínos têm sido tradicionalmente impopulares para manter a hora do sistema devido à falta de um RTC (Relógio em Tempo Real) interno. Mas com o advento de módulos RTC externos como o DS3231, eles agora têm a capacidade de manter a data e a hora precisas.

Em alternativa ao DS3231, existem outros módulos RTC, como o DS1307, que também são compatíveis com o Arduino.

Para obter a data e a hora atuais do RTC, você precisa inicializá-lo previamente com a data e a hora corretas. Isso é feito normalmente usando um sketch de configuração separado. A hora é mantida com a ajuda de uma bateria de célula de moeda, mesmo quando o Arduino está desligado. O DS3231 tem a vantagem adicional de ter uma precisão de ± 2 minutos por ano, o que o torna muito útil para projetos de longa duração.

## Veja Também:

1. Documentação oficial do Arduino: [https://www.arduino.cc/en/main/docs](https://www.arduino.cc/en/main/docs)
2. Módulo RTC DS3231: [https://lastminuteengineers.com/ds3231-rtc-arduino-tutorial/](https://lastminuteengineers.com/ds3231-rtc-arduino-tutorial/)
3. Biblioteca RTClib (usada neste artigo): [https://github.com/adafruit/RTClib](https://github.com/adafruit/RTClib)
4. Módulo alternativo RTC DS1307: [https://lastminuteengineers.com/ds1307-rtc-arduino-tutorial/](https://lastminuteengineers.com/ds1307-rtc-arduino-tutorial/)