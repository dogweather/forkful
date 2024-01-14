---
title:    "Arduino: Convertendo uma data em uma string"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que converter uma data em uma string no Arduino?

Você pode se perguntar por que alguém se daria ao trabalho de converter uma data em uma string no Arduino. A resposta é simples: para facilitar a leitura e manipulação de datas em projetos de programação.

## Como fazer isso no Arduino

Para converter uma data em uma string no Arduino, você precisará usar a biblioteca "Arduino Time". Primeiro, defina o fuso horário da sua localização utilizando a função `setTimeZone()`. Em seguida, use a função `now()` para obter a data e hora atual. Por fim, utilize a função `strftime()` para converter a data em uma string seguindo a formatação desejada.

```Arduino
#include <Time.h>
void setup() {
  setTimeZone(-5); //definir fuso horário
  time_t now = now(); //obter data e hora atual
  char dateStr[20]; //definir uma variável para armazenar a data em formato de string
  strftime(dateStr, 20, "%Y-%m-%d %H:%M:%S", now); //converter a data em string com a formatação desejada
  Serial.println(dateStr); //imprimir a data na porta serial
}
void loop() {}
```

O código acima irá imprimir a data e hora atual no formato "YYYY-MM-DD HH:MM:SS". Você pode alterar a formatação de acordo com suas necessidades, utilizando diferentes caracteres para representar os elementos da data e hora.

## Aprofundando-se na conversão de datas em strings

Além de facilitar a leitura e manipulação de datas em projetos de programação, converter uma data em uma string também permite que você armazene datas em formatos compatíveis com bancos de dados ou outros sistemas. Por exemplo, ao utilizar a formatação "YYYY-MM-DD", você pode facilmente ordenar as datas em ordem crescente ou decrescente.

Existem também outras funções disponíveis na biblioteca "Arduino Time" para manipular datas, como `day()`, `month()`, `year()`, `hour()`, `minute()` e `second()`, que retornam os elementos específicos da data e hora em formato de inteiro. Isso pode ser útil para realizar cálculos ou condições baseadas em datas.

## Veja também:

- Documentação da biblioteca "Arduino Time": https://arduino.github.io/Arduino/api/Time/
- Tutorial sobre como utilizar a biblioteca "Arduino Time": https://randomnerdtutorials.com/arduino-time-library-keep-track-time-arduino/
- Outras bibliotecas para lidar com datas e horas no Arduino: https://www.arduinolibraries.info/categories/time