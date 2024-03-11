---
date: 2024-01-20 17:46:23.874533-07:00
description: "Extrair substrings \xE9 o processo de selecionar partes espec\xEDficas\
  \ de uma string. Programadores fazem isso para manipular, analisar ou validar informa\xE7\
  \xF5es\u2026"
lastmod: '2024-03-11T00:14:20.369662-06:00'
model: gpt-4-1106-preview
summary: "Extrair substrings \xE9 o processo de selecionar partes espec\xEDficas de\
  \ uma string. Programadores fazem isso para manipular, analisar ou validar informa\xE7\
  \xF5es\u2026"
title: Extraindo substrings
---

{{< edit_this_page >}}

## O Que & Porquê?
Extrair substrings é o processo de selecionar partes específicas de uma string. Programadores fazem isso para manipular, analisar ou validar informações de acordo com a necessidade.

## Como Fazer:
No PHP, a função `substr()` te deixa pegar um pedaço da string. Aqui está um exemplo rápido:

```php
$texto = "Olá, mundo!";
$parte = substr($texto, 4, 5); // Começa na posição 4 e pega 5 caracteres
echo $parte; // Saída: "mundo"
```
E se você quiser pegar até o final, sem especificar o comprimento:

```php
$texto = "Olá, desenvolvedor!";
$final = substr($texto, 5); // Começa na posição 5 até o fim
echo $final; // Saída: "desenvolvedor!"
```

## Mergulho Profundo
A função `substr()` existe desde as primeiras versões do PHP. Uma alternativa é a função `mb_substr()`, que é mais apropriada para strings multibyte, como as em UTF-8.

Outra função, `strstr()`, também permite extrair substrings, porém ela busca por uma ocorrência específica e retorna tudo a partir dela. Aqui está um exemplo:

```php
$email = "nome@exemplo.com";
$dominio = strstr($email, '@');
echo $dominio; // Saída: "@exemplo.com"
```

Detalhes de implementação incluem a manipulação de índices negativos em `substr()`, introduzidos no PHP 7.1, que permitem começar do final da string.

## Veja Também:
Para aprender mais sobre as funções de string no PHP, dê uma olhada na documentação oficial:
- [Função substr()](https://www.php.net/manual/pt_BR/function.substr.php)
- [Função mb_substr()](https://www.php.net/manual/pt_BR/function.mb-substr.php)
- [Função strstr()](https://www.php.net/manual/pt_BR/function.strstr.php)
