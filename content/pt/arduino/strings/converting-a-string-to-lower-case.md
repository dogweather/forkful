---
date: 2024-01-20 17:37:43.092097-07:00
description: "Como Fazer: Converter strings para min\xFAsculas \xE9 uma pr\xE1tica\
  \ comum desde os prim\xF3rdios da computa\xE7\xE3o, pois as compara\xE7\xF5es de\
  \ texto s\xE3o sens\xEDveis a\u2026"
lastmod: '2024-04-05T21:53:47.174523-06:00'
model: gpt-4-1106-preview
summary: "Converter strings para min\xFAsculas \xE9 uma pr\xE1tica comum desde os\
  \ prim\xF3rdios da computa\xE7\xE3o, pois as compara\xE7\xF5es de texto s\xE3o sens\xED\
  veis a mai\xFAsculas e min\xFAsculas por padr\xE3o."
title: "Convertendo uma string para min\xFAsculas"
weight: 4
---

## Como Fazer:
```Arduino
String mensagem = "Olá, MUNDO!";
mensagem.toLowerCase();
Serial.begin(9600);
Serial.println(mensagem); // Saída: olá, mundo!
```

## Mergulho Profundo:
Converter strings para minúsculas é uma prática comum desde os primórdios da computação, pois as comparações de texto são sensíveis a maiúsculas e minúsculas por padrão. Alternativas incluem o uso de funções como `strcasecmp()` para comparações insensíveis a maiúsculas/minúsculas, mas para normalizar dados (como entradas de usuário) a conversão para minúsculas é uma operação padrão. Na implementação do método `toLowerCase()` do Arduino, cada caractere da string é verificado e se estiver no intervalo ASCII de letras maiúsculas (65 a 90), é convertido para o respectivo minúsculo adicionando 32 a seu código ASCII.

## Ver Também:
- Documentação oficial da Arduino: https://www.arduino.cc/reference/pt/language/variables/data-types/string/functions/tolowercase/
- Tutorial sobre comparação de strings no Arduino: https://www.arduino.cc/en/Tutorial/BuiltInExamples/StringComparisonOperators
