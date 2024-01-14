---
title:    "Arduino: Calcular uma data no futuro ou passado"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Por que

Não seria ótimo se pudéssemos programar nosso Arduino para calcular uma data no futuro ou no passado? Com essa funcionalidade, você pode criar projetos dinâmicos e adaptáveis, que levam em consideração diferentes datas e horários. Continue lendo para descobrir como fazer isso de maneira simples e eficiente.

## Como fazer

O cálculo de datas no Arduino envolve três etapas: coletar a data atual, adicionar ou subtrair um determinado número de dias e, por fim, formatar a data calculada. Vamos mostrar como fazer isso usando a biblioteca "Time" e uma placa Arduino Uno.

Primeiro, precisamos incluir a biblioteca "Time" em nosso código. Isso pode ser feito no início do seu sketch, usando o seguinte comando:

```Arduino
#include <Time.h>
```

Agora vamos definir uma variável que irá armazenar a data atual. Usaremos a função "now()" da biblioteca "Time" para coletar essa informação:

```Arduino
time_t data_atual = now();
```

Em seguida, definiremos uma variável que irá armazenar o número de dias que queremos adicionar ou subtrair para calcular a data desejada. Por exemplo, se quisermos calcular a data 10 dias no futuro, podemos fazer o seguinte:

```Arduino
int dias = 10;
```

Agora vem a parte interessante - vamos calcular a nova data! Usaremos a função "adjustedTime()", que permite adicionar ou subtrair um determinado número de segundos, minutos, horas, dias, semanas, meses ou anos à data atual. Passaremos a variável "dias" como argumento para essa função, para que ela possa realizar o cálculo da data desejada.

```Arduino
time_t nova_data = adjustedTime(data_atual, 0, 0, dias, 0, 0, 0);
```

E, por fim, vamos formatar essa nova data em um formato legível para nós, humanos. Existem várias maneiras de fazer isso, mas vamos usar a função "timeToStr()" da biblioteca "Time", que nos permite definir o formato da data final. Por exemplo, se quisermos que nossa data seja apresentada no formato "DD/MM/AAAA", podemos fazer o seguinte:

```Arduino
String data_formatada = timeToStr("%d/%m/%Y", nova_data);
```

E aqui está o resultado - imprimindo a nova data formatada no monitor serial:

```Arduino
Serial.println(data_formatada);
```

## Deep Dive

Para aqueles que desejam entender melhor como o cálculo de datas funciona no Arduino, aqui está uma explicação mais detalhada:

A função "adjustedTime()" realiza a soma ou subtração em segundos na data atual, de acordo com os argumentos passados. O primeiro argumento é a data atual, seguida pelo número de segundos, minutos, horas, dias, semanas, meses e anos que desejamos adicionar ou subtrair. Por exemplo, se quisermos calcular a data 2 semanas no futuro, podemos fazer o seguinte:

```Arduino
time_t nova_data = adjustedTime(data_atual, 0, 0, 14, 0, 0, 0);
```

Note que usamos "14" como argumento, pois uma semana tem 7 dias, então 2 semanas equivalem a 14 dias.

## Veja também

- Documentação oficial da biblioteca "Time" para Arduino: https://www.arduino.cc/en/Reference/Time
- Exemplo de projeto utilizando cálculo de datas no Arduino: https://www.hackster.io/4461/displaying-the-date-and-time-on-oled-using-arduino-98ff46
- Explicação detalhada sobre a função "adjustedTime()": https://www.pjrc.com/teensy/td_libs_Time.html