---
date: 2024-01-20 17:33:58.170392-07:00
description: "Como Fazer: Concatenar strings \xE9 um conceito que existe desde os\
  \ prim\xF3rdios da programa\xE7\xE3o. No contexto do Arduino, \xE9 importante ser\
  \ eficiente com\u2026"
lastmod: '2024-04-05T21:53:47.179165-06:00'
model: gpt-4-1106-preview
summary: "Concatenar strings \xE9 um conceito que existe desde os prim\xF3rdios da\
  \ programa\xE7\xE3o."
title: Concatenando strings
weight: 3
---

## Como Fazer:
```Arduino
String primeiroNome = "João";
String sobrenome = "Silva";
String nomeCompleto = primeiroNome + " " + sobrenome; 

void setup() {
  Serial.begin(9600);
}

void loop() {
  Serial.println(nomeCompleto); // Saída: João Silva
  delay(1000); // Espera 1 segundo antes de repetir
}
```

## Mergulho Profundo
Concatenar strings é um conceito que existe desde os primórdios da programação. No contexto do Arduino, é importante ser eficiente com memória, então usar a classe `String` pode ser custoso para programas maiores. Alternativas como `strcat()` da biblioteca `cstring` podem ser usadas com arrays de char para otimizar o uso de memória. Cuidado com o buffer overflow!

## Veja Também
- Documentação do Arduino sobre strings: https://www.arduino.cc/reference/en/language/variables/data-types/string/
- Tutorial sobre gerenciamento de memória no Arduino: https://www.arduino.cc/en/Tutorial/Memory
- Fórum Arduino com discussões sobre strings: http://forum.arduino.cc/index.php?board=4.0
