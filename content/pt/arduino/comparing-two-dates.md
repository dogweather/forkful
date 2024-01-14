---
title:    "Arduino: Comparando duas datas"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Por que comparar duas datas?

Comparar duas datas é uma tarefa comum em projetos de Arduino, especialmente quando se lida com temporizadores ou para verificar a diferença entre as datas atuais e as datas armazenadas em uma memória externa. Esta comparação pode ajudar a controlar eventos ou ativar dispositivos em horários específicos. Neste artigo, vamos aprender como fazer essa comparação utilizando o Arduino.

## Como fazer

Para comparar duas datas, precisamos primeiro obter as datas desejadas e depois utilizar funções de comparação para determinar se as datas são iguais, maiores ou menores.

Passo 1: Obter as datas

Usaremos a biblioteca <DateTime.h> para obter as datas desejadas. Esta biblioteca vem junto com a biblioteca <DS3231.h> para uso com o módulo de relógio DS3231. Primeiro, devemos inicializar o módulo e, em seguida, usar a função now() para obter a data atual.

```
#include <Wire.h>
#include <DS3231.h>

DS3231 rtc; // inicializa o módulo de relógio
DateTime now = rtc.now(); // obtém a data atual
```

Passo 2: Comparar as datas

Agora que temos a data atual, podemos compará-la com uma data armazenada em uma variável. Vamos supor que temos uma data desejada armazenada em uma variável chamada "data_alvo". Podemos usar as funções de comparação <, >, == para comparar as datas e tomar uma ação com base no resultado.

```
if (now < data_alvo) {
  // a data atual é menor que a data alvo
  // faça algo
}

if (now > data_alvo) {
  // a data atual é maior que a data alvo
  // faça algo
}

if (now == data_alvo) {
  // a data atual é igual à data alvo
  // faça algo
}
```

## Aprofundando

Ao comparar duas datas, é importante levar em consideração o formato em que elas estão sendo armazenadas. Geralmente, datas são armazenadas em strings ou em milissegundos desde uma data de referência. Portanto, é importante converter as datas em um formato comum antes de compará-las.

Também é importante ter em mente que algumas funções de comparação podem não funcionar corretamente com datas maiores que o ano 2038. Isso ocorre devido a limitações de armazenamento de dados em uma variável de 32 bits. Portanto, é aconselhável usar uma biblioteca de tempo como a <TimeLib.h> para lidar com datas maiores que o ano 2038.

## Veja também

- <a href="https://www.arduino.cc/reference/pt/libraries/ds3231/">Biblioteca DS3231</a>
- <a href="https://www.arduino.cc/reference/pt/language/functions/time/">Funções de tempo</a>
- <a href="https://www.arduino.cc/reference/pt/language/functions/time/millis/">Função millis()</a>