---
date: 2024-01-20 17:50:05.803938-07:00
description: "How to: Em Arduino, usamos a fun\xE7\xE3o `sprintf` para interpolar\
  \ strings. Veja o c\xF3digo e sua sa\xEDda."
lastmod: '2024-03-13T22:44:46.825275-06:00'
model: gpt-4-1106-preview
summary: "Em Arduino, usamos a fun\xE7\xE3o `sprintf` para interpolar strings."
title: Interpolando uma string
weight: 8
---

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
