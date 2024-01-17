---
title:                "Unindo strings"
html_title:           "Arduino: Unindo strings"
simple_title:         "Unindo strings"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Concatenação de strings é a ação de combinar duas ou mais strings juntas em uma única string. Os programadores frequentemente usam isso para criar mensagens mais complexas, gerar saídas personalizadas ou manipular dados de maneira mais eficiente.

## Como fazer:
```
ArduinoString primeiroNome = "Maria";
ArduinoString sobrenome = "Silva";

ArduinoString nomeCompleto = primeiroNome + " " + sobrenome;
Serial.println(nomeCompleto);

// Saída: Maria Silva
```

## Profundidade:
Concatenação de strings é uma técnica comumente usada em programação, e tem sido usada há décadas em linguagens de programação. Alguns idiomas têm recursos integrados para facilitar essa tarefa, mas no Arduino, é necessário usar um objeto ArduinoString e o operador "+". No entanto, pode ser uma técnica útil para criar saídas personalizadas e manipular dados de texto.

## Veja também:
- [Documentação oficial do Arduino sobre ArduinoString](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)
- [Tutorial sobre como concatenar strings em Arduino](https://www.tutorialspoint.com/arduino/arduino_strings.htm)
- [Fórum da comunidade do Arduino sobre concatenação de strings](https://forum.arduino.cc/index.php?topic=67143.0)