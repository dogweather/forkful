---
title:                "Comparando duas datas"
aliases:
- /pt/arduino/comparing-two-dates.md
date:                  2024-01-20T17:32:07.318832-07:00
model:                 gpt-4-1106-preview
simple_title:         "Comparando duas datas"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Comparar duas datas significa verificar se são iguais, ou determinar qual é anterior ou posterior. Programadores fazem isso para rastrear eventos, controlar prazos ou criar funções de agendamento.

## Como fazer:

Aqui está um método simples usando a biblioteca `TimeLib.h` no Arduino:
```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  // Configura duas datas
  tmElements_t data1, data2;
  data1.Year = CalendarYrToTm(2023);
  data1.Month = 3;
  data1.Day = 14;
  data2.Year = CalendarYrToTm(2023);
  data2.Month = 3;
  data2.Day = 15;
  
  // Converte para tempo Unix
  time_t t1 = makeTime(data1);
  time_t t2 = makeTime(data2);
  
  // Compara e apresenta o resultado
  if(t1 == t2) {
    Serial.println("Datas iguais");
  } else if(t1 < t2) {
    Serial.println("Data 1 é anterior à Data 2");
  } else {
    Serial.println("Data 1 é posterior à Data 2");
  }
}

void loop() {
  // este exemplo não usa loop
}
```
**Saída esperada:**
```
Data 1 é anterior à Data 2
```

## Mergulho Profundo

Históricamente, a comparação de datas é um problema clássico em programação, com desafios como anos bissextos e fusos horários. Alternativamente, sem uma biblioteca, programadores teriam de converter manualmente as datas para um formato comum antes da comparação, um processo propenso a erros. Na implementação, ao usar Arduino, se gerencia a complexidade das datas com bibliotecas robustas como `TimeLib.h`, que manipula operações de tempo sem a necessidade de reinventar a roda.

## Veja Também

- Documentação da biblioteca TimeLib: https://www.pjrc.com/teensy/td_libs_Time.html
- Tutorial do Arduino sobre o gerenciamento de tempo e datas: https://www.arduino.cc/en/Tutorial/LibraryExamples/TimeSerial
- Fórum Arduino para discussão de problemas relacionados a tempo e data: http://forum.arduino.cc/index.php?board=9.0
