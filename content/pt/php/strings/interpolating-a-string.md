---
date: 2024-01-20 17:51:14.750414-07:00
description: "Como Fazer: Se voc\xEA quiser usar chaves complexas."
lastmod: '2024-04-05T21:53:46.998986-06:00'
model: gpt-4-1106-preview
summary: "Se voc\xEA quiser usar chaves complexas."
title: Interpolando uma string
weight: 8
---

## Como Fazer:
```PHP
<?php
$planeta = "Terra";
$mensagem = "Olá, $planeta!";
echo $mensagem; // Saída: Olá, Terra!
```

Se você quiser usar chaves complexas:

```PHP
<?php
$dados = ['planeta' => 'Terra', 'satelite' => 'Lua'];
$mensagem = "Olá, {$dados['planeta']} e sua satélite, {$dados['satelite']}!";
echo $mensagem; // Saída: Olá, Terra e sua satélite, Lua!
```

## Mergulho Profundo
A interpolação de strings é algo que existe no PHP desde suas primeiras versões, facilitando a substituição de variáveis dentro de strings. Antigamente, era comum ver muita concatenação com o operador `.` – algo que pode tornar o código mais verboso e difícil de manter.

Alternativas? Sim, as template strings (heredoc e nowdoc) são uma delas. E se precisar de uma interpolação mais complexa ou a avaliação de expressões, pode-se usar a função `sprintf()` ou `printf()`.

Nos bastidores, o PHP processa strings com aspas duplas e procura por variáveis para substituir pelos seus valores. Mas cuidado: strings com aspas simples são literais, então nada de interpolação nelas.

## Veja Também
- [String interpolation in PHP](https://www.php.net/manual/en/language.types.string.php#language.types.string.parsing) - Detalhes oficiais na documentação do PHP.
- [sprintf() for formatted strings](https://www.php.net/manual/en/function.sprintf.php) - Documentação da função `sprintf()`.
- [PHP: Strings](https://www.php.net/manual/en/language.types.string.php) - Variedade de operações de strings no PHP.
- [PHP: Heredoc syntax](https://www.php.net/manual/en/language.types.string.php#language.types.string.syntax.heredoc) - Sintaxe heredoc no PHP.
- [PHP: Nowdoc syntax](https://www.php.net/manual/en/language.types.string.php#language.types.string.syntax.nowdoc) - Sintaxe nowdoc no PHP.
