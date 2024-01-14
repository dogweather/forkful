---
title:                "Arduino: Calculando uma data no futuro ou passado"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Você já se perguntou como é possível calcular uma data no futuro ou no passado usando o Arduino? Bem, essa habilidade pode ser útil em uma variedade de projetos, desde cronômetros até sistemas de agendamento. Além disso, é um ótimo exercício de programação!

## Como fazer

Para calcular uma data no futuro ou passado usando o Arduino, primeiro precisamos entender como as datas são representadas em nosso sistema de calendário. Elas são compostas por uma combinação de dia, mês e ano. Usando essa informação, podemos fazer cálculos simples para adicionar ou subtrair dias, meses ou anos a uma data específica.

Vamos começar com um exemplo básico: adicionar 1 dia a uma data. Usaremos a biblioteca "Time" do Arduino para obter a data atual e, em seguida, exibiremos a nova data após adicionar 1 dia.

```
Arduino #include <Time.h>

void setup() {
  Serial.begin(9600); //iniciar a comunicação serial
  setTime(10, 00, 00, 01, 05, 2021); //definir a data (segundos, minutos, horas, dia, mês, ano)
}

void loop() {
  time_t currentTime = now();
  time_t newTime = currentTime + 86400; //adicionar 1 dia (em segundos)
  tmElements_t newDate;
  breakTime(newTime, newDate); //converter o novo tempo para um formato compreensível
  Serial.print(newDate.Day);
  Serial.print("/");
  Serial.print(newDate.Month);
  Serial.print("/");
  Serial.println(newDate.Year);
  delay(1000);
}
```

Neste exemplo, usamos a função "setTime" para definir a data como 1 de maio de 2021. Em seguida, adicionamos 86400 segundos (1 dia) à data atual usando a função "now". Finalmente, convertemos o novo tempo usando a função "breakTime" e exibimos a data no formato dia/mês/ano.

Agora, vamos dar um passo adiante e adicionar 1 mês a uma data. O processo é semelhante, mas precisamos levar em conta a diferença nos dias entre os meses. Vamos adicionar 1 mês à mesma data do exemplo anterior.

```
Arduino #include <Time.h>

void setup() {
  Serial.begin(9600); //iniciar a comunicação serial
  setTime(10, 00, 00, 01, 05, 2021); //definir a data (segundos, minutos, horas, dia, mês, ano)
}

void loop() {
  time_t currentTime = now();
  tmElements_t newDate;
  breakTime(currentTime, newDate);

  int daysInMonth = 31; //definir número padrão de dias no mês
  if(newDate.Month == 4 || newDate.Month == 6 || newDate.Month == 9 || newDate.Month == 11){ //verificar meses com apenas 30 dias
    daysInMonth = 30;
  } else if(newDate.Month == 2){ //verificar fevereiro e ajustar para considerar anos bissextos
    if(newDate.Year % 4 == 0){
      daysInMonth = 29;
    } else {
      daysInMonth = 28;
    }
  }

  time_t newTime = currentTime + (daysInMonth * 86400); //adicionar 1 mês (em segundos)
  breakTime(newTime, newDate); //converter o novo tempo para um formato compreensível
  Serial.print(newDate.Day);
  Serial.print("/");
  Serial.print(newDate.Month);
  Serial.print("/");
  Serial.println(newDate.Year);
  delay(1000);
}
```

Aqui, usamos a função "breakTime" para obter o número de dias no mês atual. Então, adicionamos esse número multiplicado por 86400 segundos (1 dia) ao tempo atual. Finalmente, convertemos o novo tempo e exibimos a data no formato dia/mês/ano.

## Mergulho profundo

Existem muitas outras maneiras de calcular datas no futuro ou no passado usando o Arduino. Por exemplo, podemos usar a função "mktime" para criar o tempo a partir de uma data específica e, em seguida, adicionar ou subtrair segundos, minutos, horas, etc. Também podemos criar uma função personalizada que leva em consideração diferentes cenários