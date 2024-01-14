---
title:    "Arduino: Obtendo a data atual"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que utilizar a programação Arduino para obter a data atual?

Muitas vezes, em projetos de automação e controle, é necessário saber a data atual para realizar determinadas tarefas ou tomar decisões lógicas. Com a programação Arduino, é possível obter a data atual facilmente e utilizá-la em seu projeto de forma precisa e eficiente.

## Como obter a data atual utilizando o Arduino

Para obter a data atual em um projeto Arduino, é preciso utilizar a biblioteca "RTClib". Esta biblioteca permite a comunicação com um relógio de tempo real (RTC - Real-Time Clock), que é responsável por manter a data e hora atual.

Primeiro, é necessário importar a biblioteca no início do código:

```Arduino
#include <RTClib.h>
```

Em seguida, é preciso criar um objeto da classe "RTC_DS1307" que irá se comunicar com o RTC:

```Arduino
RTC_DS1307 rtc;
```

É importante, também, inicializar o RTC dentro do método "setup()" do Arduino, para garantir que a comunicação esteja funcionando corretamente:

```Arduino
void setup() {
  Serial.begin(9600);
  rtc.begin();
}
```

Agora, é possível utilizar o método "now()" da biblioteca "RTClib" para obter a data e hora atual em uma variável do tipo "DateTime". Por exemplo:

```Arduino
DateTime data = rtc.now();
```

Para visualizar a data no monitor serial, basta utilizar os métodos "day()", "month()", "year()", "hour()", "minute()" e "second()" da variável "data". Por exemplo:

```Arduino
Serial.print(data.day());
Serial.print("/");
Serial.print(data.month());
Serial.print("/");
Serial.print(data.year());
Serial.print(" ");
Serial.print(data.hour());
Serial.print(":");
Serial.print(data.minute());
Serial.print(":");
Serial.print(data.second());
```

O resultado seria algo como "25/6/2020 14:50:23".

## Aprofundando na obtenção da data atual

Caso não possua um RTC conectado ao seu Arduino, é possível utilizar as funções "day()", "month()", "year()", "hour()", "minute()" e "second()" do Arduino para obter a data atual. Porém, essa forma não é tão precisa e confiável quanto utilizando um RTC.

Além disso, é importante lembrar que, ao utilizar um RTC, é necessário manter a bateria do mesmo carregada para evitar a perda dos dados armazenados.

## Veja também

- [Documentação da biblioteca RTClib](https://github.com/adafruit/RTClib)
- [Tutorial sobre como utilizar o RTC DS1307](https://www.filipeflop.com/blog/utilizando-modulo-rtc-ds1307/)
- [Guia para iniciantes de programação Arduino](https://www.arduino.cc/en/Guide/HomePage)