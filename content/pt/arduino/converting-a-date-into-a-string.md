---
title:    "Arduino: Convertendo uma data para uma string"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Por que converter uma data em string?

Quando se trabalha com programação em Arduino, pode ser necessário converter uma data em formato de string, para poder exibi-la de maneira legível para o usuário ou para enviar por meio de comunicação serial. Isso é especialmente útil para projetos que envolvem registro de dados ou monitoramento de tempo.

## Como fazer:

```Arduino
#include <Time.h>

// Definir um objeto para a data e hora
tmElements_t tm;

// Atribuir valores para a data e hora
tm.Day = 30;
tm.Month = 10;
tm.Year = 2021;
tm.Hour = 14;
tm.Minute = 30;
tm.Second = 0;

// Converter e exibir a data e hora em formato de string
char stringData[11];
sprintf(stringData, "%02d/%02d/%04d", tm.Day, tm.Month, tm.Year);
char stringHora[6];
sprintf(stringHora, "%02d:%02d", tm.Hour, tm.Minute);
Serial.print("Data: ");
Serial.println(stringData);
Serial.print("Hora: ");
Serial.println(stringHora);

```

**Saída:**

```
Data: 30/10/2021
Hora: 14:30
```

## Detalhes mais profundos:

Ao converter uma data em string, é importante ter em mente o formato desejado para a exibição. No exemplo acima, utilizamos o formato "DD/MM/YYYY" para a data e "HH:MM" para a hora, mas é possível alterar de acordo com as necessidades do projeto.

Também é importante ter cuidado ao trabalhar com datas e horários, pois o formato pode variar de acordo com a localização e fuso horário. Para isso, é recomendável utilizar bibliotecas como "Time" ou "Timezone" para facilitar o gerenciamento de datas e horários em diferentes regiões.

Além disso, é possível adicionar informações adicionais à string, como o dia da semana ou o nome do mês em vez do número correspondente. Isso pode ser útil para deixar a exibição mais clara para o usuário final.

## Veja também:

- [Documentação da biblioteca Time para Arduino](https://www.arduino.cc/en/Reference/time) 
- [Tutorial de uso da biblioteca Timezone para Arduino](https://learn.adafruit.com/using-ntp-on-feather-rtc-setting-dst/examples-adding-timezone-to-ntp) 
- [Exemplo de conversão de data e hora em string no Arduino](https://www.instructables.com/Arduino-Data-Logging-With-Date-and-Time/)