---
date: 2024-01-20 17:35:53.747386-07:00
description: "Converter datas em strings permite que voc\xEA exiba ou armazene datas\
  \ de forma leg\xEDvel. Programadores fazem isso para facilitar a leitura e o processamento\u2026"
lastmod: '2024-03-11T00:14:20.577421-06:00'
model: gpt-4-1106-preview
summary: "Converter datas em strings permite que voc\xEA exiba ou armazene datas de\
  \ forma leg\xEDvel. Programadores fazem isso para facilitar a leitura e o processamento\u2026"
title: Convertendo uma data em uma string
---

{{< edit_this_page >}}

## O Que & Porquê?
Converter datas em strings permite que você exiba ou armazene datas de forma legível. Programadores fazem isso para facilitar a leitura e o processamento de datas no formato que preferirem.

## Como Fazer:
```arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("Couldn't find RTC");
    while (1);
  }
}

void loop() {
  DateTime now = rtc.now();

  char dateString[20];
  snprintf(dateString, sizeof(dateString), "%02d/%02d/%04d %02d:%02d:%02d", now.day(), now.month(), now.year(), now.hour(), now.minute(), now.second());

  Serial.println(dateString);
  delay(1000);
}
```
**Saída de exemplo:**
`23/03/2023 15:45:07`

## Aprofundamento
Converter datas para strings não é novidade. No passado, diversas funções como `sprintf()` ou bibliotecas como `RTClib` são utilizadas para formatar esses dados. Existem alternativas como `String()` ou uso de strings literais com operadores de concatenação, mas cuidado com o uso excessivo de memória e fragmentação. O `snprintf()` é uma escolha eficiente, pois você controla o tamanho do buffer e evita overflow.

## Veja Também:
- Documentação `RTClib`: https://github.com/adafruit/RTClib
- Referência do `snprintf()`: http://www.cplusplus.com/reference/cstdio/snprintf/
- Discussões sobre manipulação de strings em fóruns do Arduino: https://forum.arduino.cc/index.php?board=7.0

Lembrando que os links acima estão em inglês. Para informações adicionais em português, busque em fóruns brasileiros e portugueses de Arduino ou confira a documentação oficial traduzida quando disponível.
