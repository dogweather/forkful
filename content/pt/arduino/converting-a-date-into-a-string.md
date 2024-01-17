---
title:                "Convertendo uma data em uma string"
html_title:           "Arduino: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O que & Porquê?

Converter uma data em uma string é simplesmente transformar a data em um formato de texto compreensível pelos humanos. Os programadores fazem isso para facilitar a leitura e manipulação de datas em seus códigos.

## Como fazer:

Converter uma data para uma string no Arduino é bem fácil. Veja um exemplo abaixo onde a data é obtida através do módulo de tempo DS1307 e o valor é armazenado em uma variável chamada "data". Em seguida, usamos a função "String" para converter essa variável em uma string utilizando o formato "dd/mm/aaaa".

```Arduino
#include <Wire.h>
#include <DS1307.h>

DS1307 rtc;
int data;

void setup(){
    rtc.begin();
    rtc.getTime();
    data = rtc.getDate();
}

void loop(){
    String dataString = String(data, DEC);
    String dateString = dataString.substring(4, 6) + "/" + dataString.substring(2, 4) + "/" + dataString.substring(0, 2);
    Serial.println(dateString);
}
```

A saída do código acima será uma string com a data no formato "dd/mm/aaaa". 

## Deep Dive:

Historicamente, a conversão de datas em strings era realizada manualmente pelos programadores. No entanto, com o avanço da tecnologia, surgiram funções prontas que facilitam esse processo. Além disso, existem outras formas de representar datas em códigos, como o uso de números inteiros ou arrays. No entanto, a conversão para string é a mais comum e prática.

## Veja também:

- Documentação sobre a função String do Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/string/
- Exemplo de uso do módulo DS1307: https://arduinodiy.wordpress.com/2014/09/05/arduino-ds1307-lcd-clock-with-rtc-timerclock-tutorial-and-schematics/
- Artigo sobre a importância da conversão de datas em strings: https://medium.com/@githubarchive/the-importance-of-format-aab676e384dd