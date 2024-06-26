---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:52:55.519514-07:00
description: "Como fazer: No VBA, voc\xEA usa principalmente as fun\xE7\xF5es `Mid`,\
  \ `Left` e `Right` para extrair substrings. Abaixo, exploramos essas fun\xE7\xF5\
  es com exemplos: 1.\u2026"
lastmod: '2024-03-13T22:44:46.399586-06:00'
model: gpt-4-0125-preview
summary: "No VBA, voc\xEA usa principalmente as fun\xE7\xF5es `Mid`, `Left` e `Right`\
  \ para extrair substrings."
title: Extraindo substrings
weight: 6
---

## Como fazer:
No VBA, você usa principalmente as funções `Mid`, `Left` e `Right` para extrair substrings. Abaixo, exploramos essas funções com exemplos:

1. **Mid**: Extrai uma substring de uma string a partir de uma posição especificada.
   ```basic
   Dim exampleString As String
   exampleString = "Hello World"
   Dim result As String
   result = Mid(exampleString, 7, 5)
   Debug.Print result  ' Saída: World
   ```

2. **Left**: Extrai uma substring da esquerda da string, até um determinado número de caracteres.
   ```basic
   Dim exampleString As String
   exampleString = "Hello World"
   Dim result As String
   result = Left(exampleString, 5)
   Debug.Print result  ' Saída: Hello
   ```

3. **Right**: Extrai uma substring da direita da string, até um determinado número de caracteres.
   ```basic
   Dim exampleString As String
   exampleString = "Hello World"
   Dim result As String
   result = Right(exampleString, 5)
   Debug.Print result  ' Saída: World
   ```

Essas funções fundamentais formam a base da extração de substrings no VBA, oferecendo abordagens robustas e simples para a manipulação de strings.

## Aprofundamento:
Historicamente, a habilidade de manipular strings na programação tem sido essencial, com o BASIC (o progenitor do VBA) sendo um dos primeiros a democratizar essa capacidade nos primeiros dias da informática pessoal. As funções `Mid`, `Left` e `Right` no VBA herdam esse legado, oferecendo uma interface simplificada para programadores modernos.

Embora essas funções sejam bastante eficazes para muitas tarefas, o surgimento de Expressões Regulares em linguagens mais novas forneceu uma maneira mais poderosa e flexível de trabalhar com texto. Apesar disso, a simplicidade imediata e disponibilidade das funções tradicionais de substring do VBA as tornam perfeitamente adequadas para tarefas rápidas e para aqueles novos na programação.

Para operações de análise e busca mais complexas dentro de strings, o VBA também suporta correspondência de padrões através do operador `Like` e Expressões Regulares por meio do objeto `VBScript.RegExp`, embora esses exijam um pouco mais de configuração e entendimento para serem usados efetivamente. Enquanto essas ferramentas oferecem maior poder, a natureza direta de `Mid`, `Left` e `Right` garante sua relevância e utilidade contínuas em muitos programas VBA.
