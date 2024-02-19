---
aliases:
- /pt/php/converting-a-string-to-lower-case/
date: 2024-01-20 17:38:49.078502-07:00
description: "Converter uma string para letras min\xFAsculas significa transformar\
  \ todas as letras mai\xFAsculas em sua correspondente min\xFAscula. Programadores\
  \ fazem isso\u2026"
lastmod: 2024-02-18 23:08:58.228133
model: gpt-4-1106-preview
summary: "Converter uma string para letras min\xFAsculas significa transformar todas\
  \ as letras mai\xFAsculas em sua correspondente min\xFAscula. Programadores fazem\
  \ isso\u2026"
title: "Convertendo uma string para min\xFAsculas"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Converter uma string para letras minúsculas significa transformar todas as letras maiúsculas em sua correspondente minúscula. Programadores fazem isso para padronizar os dados, facilitar comparações de texto e evitar erros causados por diferenças de caixa.

## Como Fazer:

No PHP, você usa a função `strtolower()` para converter uma string para minúsculas. Aqui está um exemplo prático:

```PHP
<?php
$textoOriginal = "Olá Mundo!";
$textoMinusc = strtolower($textoOriginal);

echo $textoMinusc; // Resultado: olá mundo!
?>
```

## Mergulho Profundo:

Historicamente, a função `strtolower()` faz parte do PHP desde as primeiras versões, sendo essencial para manipulação de strings. Alternativas incluem `mb_strtolower()` quando lidamos com múltiplos conjuntos de caracteres, especialmente para suporte adequado a UTF-8 - importante para a língua portuguesa e outras que contêm caracteres além do ASCII básico.

Quando você implementa a conversão para minúsculas, lembre-se que PHP considera locale na função `strtolower()`. Isto pode afetar a saída se você estiver trabalhando com caracteres especiais. Veja um exemplo com a função `mb_strtolower()`:

```PHP
<?php
$textoOriginal = "Olá MÜNDO!";
$textoMinusc = mb_strtolower($textoOriginal, 'UTF-8');

echo $textoMinusc; // Resultado: olá mündo!
?>
```

Note que a função `mb_strtolower()` converte corretamente o "Ü" para "ü", mantendo a integridade dos caracteres especiais.

## Veja Também:

- Documentação oficial da função `strtolower()`: [strtolower - Manual PHP](https://www.php.net/manual/pt_BR/function.strtolower.php)
- Documentação oficial da função `mb_strtolower()`: [mb_strtolower - Manual PHP](https://www.php.net/manual/pt_BR/function.mb-strtolower.php)
- Página sobre Unicode e UTF-8 no PHP: [Manipulando strings Unicode no PHP](https://www.php.net/manual/pt_BR/book.mbstring.php)
