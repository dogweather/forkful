---
title:                "Arduino: Obtendo a data atual."
simple_title:         "Obtendo a data atual."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que obter a data atual é útil para programar com Arduino?

Existem muitas aplicações interessantes que podem se beneficiar da obtenção da data atual em um programa Arduino. Por exemplo, você pode criar um conjunto de luzes que são ativadas em feriados específicos, usar a data atual como um parâmetro para determinadas ações ou até mesmo criar um relógio que se ajusta automaticamente ao horário de verão e ao horário de inverno.

## Como obter a data atual com Arduino

Para obter a data atual em um programa Arduino, você precisará usar a biblioteca "DS1307RTC". Certifique-se de baixar a biblioteca e adicioná-la ao seu ambiente Arduino antes de começar.

Aqui está um exemplo de código simples que usa a biblioteca para obter a data atual e imprimi-la no monitor serial:

```arduino
#include <DS1307RTC.h> // inclui a biblioteca DS1307RTC

void setup() {
  Serial.begin(9600); // inicia a comunicação serial
  setSyncProvider(RTC.get); // obtém os dados de data e hora do chip DS1307RTC
}

void loop() {
  tmElements_t tm; // cria uma variável para armazenar os elementos da data
  if (RTC.read(tm)) { // verifica se a data foi lida com sucesso
    // imprimi a data no formato dia/mês/ano
    Serial.print(tm.Day);
    Serial.print("/");
    Serial.print(tm.Month);
    Serial.print("/");
    Serial.println(tm.Year + 1970); // o valor retornado é desde 1970
  }
  delay(1000); // espera um segundo antes de verificar novamente
}
```

No código acima, usamos a função `RTC.read()` para obter os dados da data e hora do chip DS1307RTC e armazená-los em uma variável `tmElements_t`. Em seguida, imprimimos os elementos individuais da data no monitor serial.

Agora, quando você carregar este código em seu Arduino e abrir o monitor serial, você verá a data atual sendo impressa no formato dia/mês/ano.

## Mergulho Profundo: Mais informações sobre a obtenção da data atual com Arduino

A biblioteca "DS1307RTC" também oferece outras funções úteis para lidar com a data e hora, como definir a data e hora, verificar se o horário de verão está ativo e até mesmo utilizar o chip DS1307 como um relógio de alarme.

Se você quiser saber mais sobre como usar essas funções, confira a documentação oficial da biblioteca DS1307RTC.

## Veja também

- [Tutorial da biblioteca DS1307RTC](https://github.com/PaulStoffregen/DS1307RTC)
- [Referência da biblioteca DS1307RTC](https://www.arduino.cc/en/reference/ds1307rtc)
- [Guia de Início Rápido do chip DS1307](https://www.adafruit.com/product/264)