---
title:                "Interpolando uma string"
aliases:
- /pt/arduino/interpolating-a-string/
date:                  2024-01-20T17:50:05.803938-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolando uma string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Interpolar uma string é encaixar valores de variáveis dentro dela. Programadores usam isso para facilitar a montagem de mensagens dinâmicas ou para exibir dados variáveis de forma legível.

## How to:
Em Arduino, usamos a função `sprintf` para interpolar strings. Veja o código e sua saída:

```Arduino
char buffer[50];
int temperatura = 23;
sprintf(buffer, "A temperatura atual é %d°C", temperatura);
Serial.begin(9600);
Serial.println(buffer);
```
Saída:
```
A temperatura atual é 23°C
```

## Deep Dive
A função `sprintf` vem da linguagem C, usada para formatar strings há décadas. Alternativas incluem a concatenação manual de strings mas isso é menos prático. Na interpolação via `sprintf`, você define um template com "placeholders" como `%d` para inteiros. Cuidado com o tamanho do buffer para evitar overflow!

## See Also
- Documentação Arduino `sprintf`: https://www.arduino.cc/reference/en/language/functions/communication/serial/print/
- Tutorial C `sprintf`: https://www.cplusplus.com/reference/cstdio/sprintf/
