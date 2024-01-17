---
title:                "Obtendo a data atual"
html_title:           "Arduino: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O que & Porquê?
Obter a data atual é uma tarefa comum para programadores que desejam automatizar certas ações com base na data do sistema. Ela permite que o programa execute diferentes ações dependendo da data atual.

## Como fazer:
Arduino possui a biblioteca ```Time.h``` que permite obter a data e hora atual do sistema com o uso da função ```now()```. Veja um exemplo de código abaixo:
```Arduino
#include <Time.h>

void setup(){
  Serial.begin(9600); //Inicia comunicação com o monitor serial
  setTime(now()); //Define a data e hora atual do sistema
}

void loop(){
  //Obtém a data e hora atual
  time_t t = now();
  //Exibe a data e hora no monitor serial
  Serial.println(ctime(&t));
  //Espera 1 segundo antes de repetir
  delay(1000);
}
```
A saída deste código será algo como:
```
Fri Mar 20 12:23:02 2020
```

## Aprofundando:
Para o correto funcionamento da função ```now()```, é necessário ter um relógio em tempo real (RTC) conectado ao Arduino. Existem várias opções de RTC no mercado, incluindo módulos que podem ser alimentados pela porta USB do Arduino. Além disso, é possível trabalhar com diferentes formatos de data e hora, como Unix time ou um formato personalizado.

## Veja também:
- Time.h biblioteca do Arduino: https://www.arduino.cc/en/reference/time
- Módulos RTC para Arduino: https://www.arduino.cc/search?q=RTC