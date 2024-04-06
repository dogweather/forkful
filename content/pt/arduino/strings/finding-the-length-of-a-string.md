---
date: 2024-01-20 17:46:48.145401-07:00
description: "How to: A fun\xE7\xE3o `.length()` \xE9 sua amiga aqui. Inicia a comunica\xE7\
  \xE3o serial, envia o tamanho. Olha s\xF3, 13 caracteres."
lastmod: '2024-04-05T21:53:47.178306-06:00'
model: gpt-4-1106-preview
summary: "A fun\xE7\xE3o `.length()` \xE9 sua amiga aqui."
title: Descobrindo o comprimento de uma string
weight: 7
---

## How to:
```Arduino
String texto = "Olá, Arduino!";
int tamanho = texto.length();
Serial.begin(9600);
Serial.println(tamanho);  // Saída: 13
```
A função `.length()` é sua amiga aqui. Inicia a comunicação serial, envia o tamanho. Olha só, 13 caracteres.

## Deep Dive
Antes de `String`, usávamos `char[]` - vetores de caracteres - em C puro, onde se contava manualmente com loops. `String` facilitou demais a vida.

Alternativas? Há `strlen()` para `char[]` e outras bibliotecas de manipulação de strings, mas `String` já está aqui pra isso.

Sobre a implementação, `String` em Arduino usa sobre carga de operador e gerenciamento de memória próprio. Ao buscar seu tamanho, você não itera pelo texto; a classe conhece esse valor internamente.

Cuidado com a memória do Arduino. `String` pode causar fragmentação ao mudar de tamanho. Strings estáticas ou `char[]` podem salvar o dia em projetos críticos.

## See Also
- Documentação oficial do Arduino sobre Strings: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Tutorial sobre gerenciamento de Strings e char arrays: https://www.arduino.cc/en/Tutorial/BuiltInExamples/StringCharacters
- Discussão sobre o uso de String vs char[] para otimização de memória: https://arduino.stackexchange.com/questions/13545/why-should-i-not-use-the-string-class-in-arduino
