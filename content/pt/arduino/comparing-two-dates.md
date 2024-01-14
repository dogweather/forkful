---
title:                "Arduino: Comparando duas datas"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar datas em programação Arduino?

Saber como comparar datas em programação Arduino pode ser extremamente útil em diversas aplicações. Por exemplo, pode-se utilizar essa técnica para controlar eventos que acontecem em horários específicos, para programar a iluminação de um ambiente ou até mesmo para criar alarmes.

## Como comparar datas em programação Arduino

Para comparar duas datas em um código Arduino, é necessário seguir alguns passos simples:

1. Definir as variáveis para as duas datas que serão comparadas, utilizando o tipo de dados `DateTime` da biblioteca `RTClib`.
2. Utilizar o método `now()` da biblioteca `RTClib` para obter a data e hora atual do sistema.
3. Utilizar os métodos `day()`, `month()` e `year()` para obter os valores de dia, mês e ano da data atual.
4. Utilizar uma estrutura condicional (if/else) para comparar os valores de dia, mês e ano das duas datas e realizar a ação desejada.

Abaixo, segue um exemplo de código que compara duas datas e acende um LED caso as datas sejam iguais:

```
#include <RTClib.h> 

RTC_DS1307 rtc;
DateTime data1, data2;

int led = 13;

void setup() {
  Serial.begin(9600);
  pinMode(led, OUTPUT);

  if (! rtc.begin()) {
    Serial.println("O módulo RTC não foi encontrado!");
    while (1);
  }

  if (! rtc.isrunning()) {
    Serial.println("O módulo RTC não está funcionando corretamente!");
  }

  data1 = DateTime(2020, 10, 25, 10, 0, 0); //primeira data para comparação
  data2 = DateTime(rtc.now()); //segunda data para comparação é a data e hora atuais
}

void loop() {

  if (data1.day() == data2.day() && data1.month() == data2.month() && data1.year() == data2.year()) { //compara os valores de dia, mês e ano das duas datas
    digitalWrite(led, HIGH); //se as datas forem iguais, acende o LED
    Serial.println("As datas são iguais!");
  } else {
    digitalWrite(led, LOW); //se as datas não forem iguais, mantém o LED apagado
    Serial.println("As datas são diferentes!");
  }
  delay(500);
}
```

O código acima utiliza a biblioteca `RTClib` para obter a data e hora atual e compara com a data definida pela variável `data1`. Caso as datas sejam iguais, o LED é aceso e uma mensagem é exibida no monitor serial.

## Aprofundando na comparação de datas

É importante ressaltar que, ao comparar datas, é necessário levar em consideração o formato escolhido para exibir a data. No exemplo acima, utilizamos o formato `DateTime(Y, M, D, h, m, s)`, onde Y representa o ano, M o mês, D o dia, h a hora, m o minuto e s o segundo. No entanto, existem outras formas de representar e comparar datas, como por exemplo utilizando a biblioteca `Time` e seus respectivos métodos `hour()`, `minute()` e `second()`.

Além disso, é importante considerar os formatos de hora e data utilizados pelo RTC (Real Time Clock), que podem variar dependendo do tipo de modelo utilizado.

## Veja também

- [Tutorial completo: como utilizar datas e horas no Arduino](https://www.arduino.cc/reference/en/libraries/rtclib/)
- [Como utilizar a biblioteca RTClib](https://www.arduino.cc/en/Reference/RTClib)
- [Documentação oficial da biblioteca Time](https://www.pjrc.com/teensy/td_libs_Time.html)