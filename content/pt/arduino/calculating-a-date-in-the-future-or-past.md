---
title:                "Calculando uma data no futuro ou passado"
html_title:           "Arduino: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Porquê e para quê?

Calcular uma data no futuro ou passado é a ação de determinar um dia específico com base em um ponto de referência de tempo definido. Programadores usam isto para funções como avaliar períodos de tempo ou projetar prazos.

## Como fazer:

Vamos implementar esta função na plataforma Arduino usando a biblioteca TimeLib. A biblioteca TimeLib é uma grande ferramenta para manipulação de tempo e datas no Arduino.

Instale a biblioteca através do gerenciador de bibliotecas do Arduino, procurando por "TimeLib".

Agora vamos calcular uma data exatamente uma semana no futuro de uma data inicial.

```Arduino
#include <TimeLib.h>  

void setup() {
  Serial.begin(9600);
  
  setTime(12, 0, 0, 1, 1, 2022); // Configuração de tempo inicial (Hora, Minuto, Segundo, Dia, Mês, Ano)
}

void loop() {
  time_t currentTime = now(); // Tempo atual ou seja, tempo inicial definido
  time_t futureTime = currentTime + SECS_PER_WEEK; // Calcutando exatamente uma semana no futuro

  Serial.println("Data atual: ");
  Serial.print(day(currentTime));
  Serial.print("/"); 
  Serial.print(month(currentTime));
  Serial.print("/");
  Serial.println(year(currentTime));
  
  Serial.println("Data no futuro: ");
  Serial.print(day(futureTime));
  Serial.print("/"); 
  Serial.print(month(futureTime));
  Serial.print("/");
  Serial.println(year(futureTime));

  delay(5000);

}
```

## Um pouco mais:

Calcular datas futuras ou passadas é uma prática que tem sido utilizada desde a era dos mainframes. É essencial em muitas tarefas de programação, principalmente nos campos de cronometragem e agendamento.

Existem outras bibliotecas e aproximações para realizar estas operações, sendo a TimeLib a mais recomendada para Arduino pela sua simplicidade e robustez.

Quando calculamos uma data futura como fizemos acima, estamos basicamente adicionando o número de segundos correspondentes ao período de tempo desejado (uma semana neste caso) ao tempo inicial definido.

Contudo, é importante ter em consideração que a biblioteca TimeLib não leva em conta o horário de verão ou os fusos horários. Portanto, caso seja sensível a estes aspetos, serão necessários ajustes adicionais.

## Ver também:

- Biblioteca TimeLib no GitHub: https://github.com/PaulStoffregen/Time
- Documentação oficial Arduino: https://www.arduino.cc/en/guide/libraries

Neste artigo, nós cobrimos o básico de como calcular datas futuras ou passadas no Arduino. A partir daqui, esperamos que possam explorar mais e implementar funcionalidades mais complexas e adaptadas às suas necessidades!