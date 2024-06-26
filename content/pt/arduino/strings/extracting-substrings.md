---
date: 2024-01-20 17:44:54.134333-07:00
description: "Como Fazer: Extrair substrings \xE9 um conceito antigo, aparecendo em\
  \ linguagens mais velhas como C e Java. No Arduino, usamos m\xE9todos da classe\
  \ String para\u2026"
lastmod: '2024-04-05T22:51:00.072149-06:00'
model: gpt-4-1106-preview
summary: "Extrair substrings \xE9 um conceito antigo, aparecendo em linguagens mais\
  \ velhas como C e Java."
title: Extraindo substrings
weight: 6
---

## Como Fazer:
```Arduino
String texto = "Olá, Mundo!";
String subtexto = texto.substring(0, 4); // Pega os caracteres de 0 a 3

Serial.begin(9600);
Serial.println(subtexto); // Saída: Olá,
```

Um exemplo mais dinâmico:

```Arduino
String frase = "Arduino é demais!";
int inicio = frase.indexOf('é');
int fim = frase.indexOf('!', inicio);

String extrato = frase.substring(inicio, fim + 1); 

Serial.begin(9600);
Serial.println(extrato); // Saída: é demais!
```

## Mergulho Profundo:
Extrair substrings é um conceito antigo, aparecendo em linguagens mais velhas como C e Java. No Arduino, usamos métodos da classe String para facilitar esse processo. As alternativas incluem usar arrays de char e manipular diretamente a memória, o que pode ser mais rápido, mas também mais propenso a erros. A implementação no Arduino cuida da alocação de memória e de evitar problemas como estouro de buffer, tornando a extração de substrings mais segura e fácil, especialmente para quem está começando.

## Veja Também:
- Documentação Arduino sobre a classe String: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Tutorial sobre manipulação de strings em C (uma base para o entendimento das strings no Arduino): https://www.cprogramming.com/tutorial/string.html
- Post no fórum do Arduino sobre boas práticas em manipulação de strings: https://forum.arduino.cc/index.php?topic=396450.0
