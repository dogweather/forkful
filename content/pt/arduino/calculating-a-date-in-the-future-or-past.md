---
title:                "Calculando uma data no futuro ou no passado"
html_title:           "Arduino: Calculando uma data no futuro ou no passado"
simple_title:         "Calculando uma data no futuro ou no passado"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Você já se perguntou como seria calcular uma data no futuro ou no passado usando a sua placa Arduino? Talvez você precise automatizar uma tarefa que dependa da data ou simplesmente queira aprender mais sobre programação. Independentemente do motivo, calcular datas pode ser uma habilidade útil ao utilizar a sua placa Arduino.

## Como fazer

Aqui está um exemplo de como calcular uma data no futuro usando o Arduino:

```
Arduino /* Código para calcular uma data no futuro */
#include <TimeLib.h>

int day = 19;
int month = 2;
int year = 2022;
int daysToAdd = 30; // dias para adicionar à data atual

// Obtém a data atual
int currentDay = day();
int currentMonth = month();
int currentYear = year();

// Calcula a nova data
int futureDay = day + daysToAdd;
int futureMonth = month;
int futureYear = year;

// Verifica se a data é válida e faz os ajustes necessários
if (futureDay > 31) {
  futureDay -= 31;
  futureMonth++;
}

if (futureMonth > 12) {
  futureMonth -= 12;
  futureYear++;
}

// Imprime a nova data
Serial.print("Data no futuro: ");
Serial.print(futureDay);
Serial.print("/");
Serial.print(futureMonth);
Serial.print("/");
Serial.println(futureYear);
```

No exemplo acima, utilizamos a biblioteca TimeLib para obter a data atual e, em seguida, adicionamos a quantidade desejada de dias à data. Também verificamos se a data resultante é válida e fazemos os ajustes necessários caso seja necessário. Por fim, imprimimos a nova data na porta serial para visualização.

Para calcular uma data no passado, basta substituir a variável `daysToAdd` por um valor negativo.

## Aprofundando

Ao calcular datas no Arduino, é importante considerar que a biblioteca TimeLib utiliza o formato de data e hora do Unix. Isso significa que o seu código deve estar ciente do número de segundos desde 1 de janeiro de 1970, conhecido como "epoch". Além disso, é preciso levar em conta fatores como anos bissextos e diferentes números de dias em cada mês.

Uma forma de evitar esses problemas é utilizar uma biblioteca de datas específica para o Arduino, que irá simplificar o processo de cálculo e lidar com essas questões automaticamente.

## Veja também

- Documentação da biblioteca TimeLib: https://github.com/PaulStoffregen/Time
- Biblioteca DS3231RTC para trabalhar com datas e horários no formato do Arduino: https://github.com/JChristensen/DS3232RTC