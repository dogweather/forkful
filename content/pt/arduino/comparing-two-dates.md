---
title:                "Comparando duas datas"
html_title:           "Arduino: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O que & Por quê?

A comparação de duas datas é um processo no qual os programadores comparam duas datas para determinar qual é a mais recente ou se ambas as datas são iguais. Isso é importante para garantir que os dados sejam organizados corretamente e para garantir que os cálculos e decisões tomadas pelo programa sejam precisos.

## Como fazer:

Para comparar duas datas no Arduino, podemos usar a função `millis ()`, que retorna o número de milissegundos desde que a placa foi ligada. Podemos usar essa função para calcular a diferença entre duas datas e determinar qual é a mais recente. Aqui está um exemplo de código que compara duas datas e imprime a mais recente:

```Arduino
unsigned long data1 = millis (); // primeira data
delay (1000); // esperar 1 segundo
unsigned long data2 = millis (); // segunda data
if (data1 > data2) {
    Serial.println ("Data 1 é mais recente!");
} else if (data2 > data1) {
    Serial.println ("Data 2 é mais recente!");
} else {
    Serial.println ("As datas são iguais!");
}
```
Saída:
```
Data 2 é mais recente!
```

## Mergulho profundo:

A comparação de duas datas é um conceito fundamental na programação e é usado em várias linguagens de programação, não apenas no Arduino. Outra forma de comparar datas é usando a estrutura `tm` da biblioteca `time.h`. Também é importante estar atento aos diferentes formatos de datas em diferentes regiões do mundo, como o formato MM/DD/YYYY nos Estados Unidos e o formato DD/MM/YYYY na Europa.

## Veja também:

- [Documentação do Arduino sobre a função `millis()`](https://www.arduino.cc/reference/en/language/functions/time/millis/)
- [Tutorial sobre como comparar datas em C](https://www.includehelp.com/c-programs/compare-two-dates.aspx)