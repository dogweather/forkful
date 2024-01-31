---
title:                "Obtendo a data atual"
date:                  2024-01-20T15:13:18.331158-07:00
simple_title:         "Obtendo a data atual"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Saber a data atual é útil para registrar eventos ou realizar ações programadas. Programadores o fazem para adicionar contexto temporal aos projetos ou controlar eventos que dependem da data.

## How to:
Para pegar a data atual no Arduino, usamos um módulo RTC (Real Time Clock), como o DS3231. Segue um exemplo de código para ler a data:

```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("Não foi possível encontrar o RTC");
    while (1);
  }
  if (rtc.lostPower()) {
    Serial.println("RTC perdeu a energia, vamos definir a data e hora!");
    // A linha abaixo ajusta a data e hora para a data e hora da compilação do sketch
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();
  
  Serial.print("Data atual: ");
  Serial.print(now.day());
  Serial.print('/');
  Serial.print(now.month());
  Serial.print('/');
  Serial.println(now.year());
  
  delay(1000);
}
```

Saída de exemplo:
```
Data atual: 31/3/2023
```

## Deep Dive
O uso de módulos RTC é comum em projetos com Arduino para adicionar a função de relógio em tempo real. O DS3231 é popular por sua precisão e facilidade de uso. Alternativas incluem o DS1307, que é menos preciso. Implementar a data atual envolve comunicar-se com o módulo via I2C e interpretar os dados. É importante notar que configuramos a data e hora automaticamente para a ocasião da compilação; ainda assim, pode-se ajustar manualmente se necessário.

Em termos históricos, antes dos módulos RTC, era complexo e impreciso manter a noção de tempo em sistemas embarcados. Hoje, com módulos RTC acessíveis, o controle temporal é uma questão de algumas linhas de código.

## See Also
- [Documentação da biblioteca RTClib](https://github.com/adafruit/RTClib)
- [Guia sobre módulos RTC com Arduino](https://learn.adafruit.com/adafruit-ds3231-precision-rtc-breakout)
