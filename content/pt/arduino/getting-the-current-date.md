---
title:                "Arduino: Obtendo a data atual."
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que obter a data atual?

Você já teve a necessidade de saber a data atual em um projeto de Arduino? Talvez você queira criar um sistema de controle de acesso baseado no dia da semana ou exibir a data em um display LCD. Independentemente do motivo, obter a data atual pode ser uma tarefa útil e relativamente simples de programar em Arduino.

## Como obter a data atual no Arduino

Para obter a data atual em seu projeto de Arduino, você precisará utilizar a biblioteca "RTClib". Esta biblioteca permite que o Arduino se conecte a um circuito de tempo real (RTC) para obter a data e hora atual. 

Primeiro, vamos importar a biblioteca "RTClib" para o nosso projeto, o que pode ser feito adicionando o seguinte código no início do seu sketch:

```Arduino
#include <RTClib.h>
```

Em seguida, vamos declarar uma variável do tipo RTC_DS1307, que é o tipo de RTC mais comum:

```Arduino
RTC_DS1307 rtc;
```

Agora precisamos inicializar o RTC no setup() do nosso sketch. Para fazer isso, podemos usar o método begin(), que verifica se o RTC está funcionando corretamente. Se estiver tudo bem, ele irá retornar "true" e, caso contrário, irá retornar "false". Por exemplo:

```Arduino
void setup() {
  rtc.begin();

  if (!rtc.begin()) {
    Serial.println("RTC não está funcionando!");
    while (1);
  }
}
```

Após a inicialização, podemos utilizar o método now() para obter a data e hora atuais. Este método retorna um objeto do tipo DateTime que possui métodos úteis para acessar os diferentes componentes da data e hora. Por exemplo, para obter o ano:

```Arduino
DateTime now = rtc.now();
int ano = now.year();
```

Agora você pode utilizar as informações de data e hora em seu projeto como desejar!

## Mergulho Profundo

Caso queira explorar mais recursos da biblioteca "RTClib", você pode verificar a documentação oficial no GitHub (https://github.com/adafruit/RTClib). Lá você encontrará informações sobre como configurar um RTC, como modificar a data e hora e muito mais.

## Ver também

- [Documentação oficial da biblioteca RTClib](https://github.com/adafruit/RTClib)
- [Tutorial em vídeo sobre como obter a data atual em Arduino](https://www.youtube.com/watch?v=j1_BlIYOxgQ)
- [Projeto de controle de acesso utilizando a data atual em Arduino](https://create.arduino.cc/projecthub/Richard7i/arduino-controle-de-acesso-usando-lcd-rtc-y-arduino-uno-e1674d)

Espero que este artigo tenha sido útil para você. Agora você pode facilmente obter a data atual em qualquer projeto de Arduino que precisar. Experimente e divirta-se criando!