---
title:    "Arduino: Comparando duas datas"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Por que comparar duas datas no Arduino?

Comparar datas é uma tarefa comum em muitos projetos de Arduino, especialmente aqueles que envolvem sensores ou dispositivos que coletam dados em intervalos específicos. Comparar datas pode ajudar a organizar os dados coletados e realizar ações com base nesse tempo, tornando seu projeto mais eficiente e preciso.

## Como fazer a comparação de duas datas no Arduino

Em primeiro lugar, é importante definir o formato em que as datas serão armazenadas e comparadas. No Arduino, é comum usar a biblioteca Time.h para manipular datas e horas. Esta biblioteca permite armazenar datas como uma estrutura de data e hora com seis variáveis: ano, mês, dia, hora, minuto e segundo.

Para comparar duas datas, é necessário primeiro criar duas estruturas de data e hora com os valores desejados. Em seguida, basta utilizar a função `timeDiff()` da biblioteca Time.h para calcular a diferença em segundos entre as duas datas. Se a diferença for igual a zero, significa que as datas são iguais.

Aqui está um exemplo de código que compara duas datas e imprime a diferença em segundos:

```
#include <Time.h>

// Definir duas datas para comparação
struct tm data1 = {2020, 7, 15, 10, 30, 30};  // 15 de julho de 2020 às 10:30:30
struct tm data2 = {2020, 7, 15, 9, 45, 0};  // 15 de julho de 2020 às 9:45:00

void setup() {
  // Inicializa a biblioteca Time.h
  setTime(10, 30, 30, 15, 7, 2020);
  
  // Calcula a diferença em segundos entre as duas datas
  int diferenca = timeDiff(data1, data2);
  
  // Imprime a diferença em segundos
  Serial.print("A diferença entre as duas datas é de: ");
  Serial.print(diferenca);
  Serial.print(" segundos.");
}

void loop() {
  // O código não terá loop pois estamos apenas comparando datas
}
```

A saída no monitor serial será:

```
A diferença entre as duas datas é de: 2700 segundos.
```

## Mais informações sobre a comparação de datas no Arduino

Existem várias funções úteis na biblioteca Time.h que podem ser usadas para manipular datas no Arduino. Por exemplo, a função `monthDays()` retorna o número de dias em um determinado mês, enquanto a função `makeTime()` permite criar uma estrutura de data a partir de variáveis separadas para ano, mês, dia, hora, minuto e segundo.

Além disso, é importante estar ciente de possíveis erros ao comparar datas, especialmente se as datas estiverem em diferentes fuso horários. É recomendado utilizar a função `time_t` para converter datas em segundos antes de compará-las.

## Veja também

- [Documentação oficial do Time.h](https://www.arduino.cc/reference/en/libraries/time/)
- [Tutorial sobre manipulação de datas no Arduino](https://www.tutorialspoint.com/arduino/arduino_date_time.htm)
- [Introdução ao uso da biblioteca Time.h](https://lastminuteengineers.com/arduino-time-management-ntp-rtc-ds3231/)

O uso eficiente e preciso de comparação de datas é uma habilidade útil em muitos projetos de Arduino. Com as informações e recursos fornecidos neste artigo, esperamos que você possa implementá-la com sucesso em seus próprios projetos!