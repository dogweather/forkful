---
title:                "Obtendo a data atual"
aliases:
- pt/arduino/getting-the-current-date.md
date:                  2024-02-03T19:08:49.355621-07:00
model:                 gpt-4-0125-preview
simple_title:         "Obtendo a data atual"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O que & Por quê?
Obter a data atual em projetos Arduino envolve a obtenção de informações em tempo real que podem ser cruciais para registro, marcação de tempo ou agendamento de tarefas. Programadores frequentemente precisam dessa capacidade para melhorar a funcionalidade, garantir a relevância dos dados e facilitar operações sensíveis ao tempo em seus projetos de IoT e embarcados.

## Como fazer:
O próprio Arduino não possui um método embutido para buscar diretamente a data atual, pois ele não tem um relógio de tempo real (RTC). Porém, isso pode ser alcançado usando módulos RTC externos, como o DS3231, e bibliotecas como a `RTClib`, desenvolvida pela Adafruit, que tornam a interface com esses módulos simples.

Primeiro, certifique-se de que a biblioteca `RTClib` está instalada no seu Arduino IDE. Depois, conecte o seu módulo RTC ao seu Arduino de acordo com a documentação dele.

Aqui está um exemplo simples para começar:

```cpp
#include <Wire.h>
#include "RTClib.h"

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);

  if (!rtc.begin()) {
    Serial.println("Não foi possível encontrar o RTC");
    while (1);
  }

  if (rtc.lostPower()) {
    Serial.println("RTC perdeu energia, vamos ajustar a hora!");
    // Quando a hora precisa ser ajustada em um novo dispositivo ou após a perda de energia, você pode fazer isso aqui.
    // rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();

  Serial.print("Data Atual: ");
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.println(now.day(), DEC);

  delay(3000); // Atraso de 3 segundos para reduzir o spam serial
}
```

Saída de amostra (assumindo que seu RTC foi previamente ajustado):

```
Data Atual: 2023/4/15
```

Este código inicializa o módulo RTC e, então, no loop, busca e imprime a data atual no Monitor Serial a cada 3 segundos. Lembre-se, a linha `rtc.adjust(...)` pode ser descomentada e modificada para ajustar inicialmente a data e a hora do RTC ou após ele ter perdido energia.
