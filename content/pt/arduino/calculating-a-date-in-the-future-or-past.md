---
title:                "Arduino: Calculando uma data no futuro ou no passado"
simple_title:         "Calculando uma data no futuro ou no passado"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Muitos projetos de programação com Arduino exigem que se calcule uma data no futuro ou no passado. Isso pode ser útil para agendar tarefas, ativar dispositivos em determinados dias ou até mesmo como parte de um jogo.

## Como Fazer

Para calcular uma data em Arduino, primeiro precisamos incluir a biblioteca "TimeLib.h". Em seguida, podemos usar a função "makeTime" que recebe seis parâmetros: ano, mês, dia, hora, minuto e segundo. Aqui está um exemplo para calcular uma data no futuro, 1 hora a partir do momento atual:

```Arduino
#include <TimeLib.h>

int year = year();
int month = month();
int day = day();
int hour = hour();
int minute = minute();
int second = second() + 3600; // adiciona 1 hora (3600 segundos)

time_t futureTime = makeTime(year, month, day, hour, minute, second);
```

Agora podemos usar a função "dayOfWeek" para obter o dia da semana correspondente à data calculada e usá-lo em nosso projeto.

```Arduino
int dow = dayOfWeek(futureTime); // 1 = Domingo, 2 = Segunda-feira, etc.
```

Podemos também usar a função "monthShortStr" para obter o mês abreviado em formato de texto.

```Arduino
String monthAbreviado = monthShortStr(month); // "Jan", "Fev", etc.
```

## Mergulho Profundo

Mas como a função "makeTime" realmente funciona? Essa função utiliza o formato "Unix time" que conta o número de segundos passados desde 1 de janeiro de 1970. Isso significa que podemos calcular datas futuras ou passadas a partir de um ponto de referência fixo. É importante lembrar que essa função só pode calcular datas até o ano 2038, devido a limitações do tipo de dado usado.

Podemos também usar outras funções da biblioteca TimeLib para obter informações como a hora atual, o dia da semana, o número de dias em um mês, entre outros. Essa biblioteca é muito útil para lidar com datas e tempos em projetos com Arduino.

## Veja Também

- Documentação Oficial da biblioteca TimeLib: https://www.arduino.cc/en/Reference/Time
- Tutorial sobre como utilizar datas em projetos com Arduino: https://www.arduino.cc/en/Tutorial/DS1302RealTimeClock
- Vídeo explicando a função "makeTime": https://www.youtube.com/watch?v=KmSRJcwXnp4