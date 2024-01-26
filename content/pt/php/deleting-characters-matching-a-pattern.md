---
title:                "Excluindo caracteres que correspondem a um padrão"
date:                  2024-01-20T17:42:56.934986-07:00
model:                 gpt-4-1106-preview
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?
Deletar caracteres que correspondem a um padrão é basicamente filtrar sua string com base em regras específicas. Programadores fazem isso para limpar dados, remover caracteres indesejados ou validar entradas.

## Como Fazer:
Imagine que você quer tirar todos os dígitos de uma string. Você pode usar a função `preg_replace` do PHP:

```php
<?php
$texto = "Alô! Ano Novo de 2023 chegando!";
$resultado = preg_replace('/\\d+/', '', $texto);

echo $resultado;
// Saída: Alô! Ano Novo de  chegando!
?>
```

Ou se você precisa remover pontuação:

```php
<?php
$texto = "Bem-vindo(a), caro(a) leitor(a)! Aproveite.";
$resultado = preg_replace('/[[:punct:]]/', '', $texto);

echo $resultado;
// Saída: Bemvindoa caroa leitora Aproveite
?>
```

## Aprofundando
A função `preg_replace` usa expressões regulares, que são um conjunto de caracteres especiais que definem um padrão de pesquisa. Elas são usadas desde os primórdios da computação para processamento de texto e continuam essenciais.

Alternativas à `preg_replace` incluem `str_replace` (para substituições simples sem o uso de padrões) e `str_ireplace` (o mesmo que `str_replace`, mas insensível a maiúsculas e minúsculas). Alguns frameworks PHP também oferecem suas próprias ferramentas para trabalhar com strings, como o Laravel com suas Facades de `Str` e `Stringable`.

Quando usar `preg_replace`, lembre-se de que ele pode ter um impacto na performance quando aplicado a strings muito grandes ou padrões complexos, pois a correspondência de padrões via expressões regulares é computacionalmente intensiva.

## Veja Também
- [Documentação oficial do PHP sobre `preg_replace`](https://www.php.net/manual/pt_BR/function.preg-replace.php)
- [Tutorial Interativo de Expressões Regulares](https://regex101.com/)
- [Documentação do Laravel sobre Strings](https://laravel.com/docs/8.x/helpers#method-str-replace-array)
