---
title:                "Extraindo substrings"
date:                  2024-01-20T17:44:54.134333-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extraindo substrings"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Extrair substrings significa pegar partes específicas de uma string. Programadores fazem isso para manipular, analisar ou validar dados de texto de forma mais eficiente.

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
