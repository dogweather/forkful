---
title:    "PHP: Buscando e substituindo texto"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Por que

Se você está trabalhando com PHP, provavelmente já se deparou com a tarefa de encontrar e substituir texto em seus códigos. Esta é uma habilidade crucial para qualquer programador e pode economizar muito tempo e esforço durante o desenvolvimento. Neste post, vamos explorar algumas técnicas para buscar e substituir textos de forma eficiente em PHP.

## Como fazer

Aqui estão algumas maneiras de realizar a tarefa de substituir texto em PHP:

```
<?php

// Usando a função str_replace()
$string = "Olá mundo!";
echo str_replace("mundo", "amigos", $string); // Saída: Olá amigos!

// Usando expressões regulares (regex)
$string = "Meu nome é João";
echo preg_replace("/João/", "José", $string); // Saída: Meu nome é José
```
Note que ambas as opções são muito similares, mas a função `str_replace()` é mais simples e eficiente para casos em que não há necessidade de trabalhar com regex. Porém, se você precisa realizar um padrão mais complexo de busca e substituição, as expressões regulares são a melhor opção.

## Mergulho profundo

Agora, vamos explorar um pouco mais sobre como as expressões regulares funcionam no PHP. A função `preg_replace()` utiliza uma sintaxe similar a outras linguagens de programação, como JavaScript e Python. Você pode utilizar operadores como `+` para encontrar uma ou mais ocorrências e `*` para encontrar qualquer número de ocorrências. Por exemplo, `/[aeiou]+/` irá encontrar qualquer vogal repetida em uma palavra.

Também é possível utilizar grupos de captura para retornar partes específicas da string original. Por exemplo, se você quiser substituir um e-mail por asteriscos, pode utilizar o seguinte código:
```
<?php

$string = "Meu e-mail é joao@email.com";
echo preg_replace("/(e-mail\sé\s)((\w+|-|\.)+)+(@)(((\w+|-)+\.))+(\w\w+)/", "$1***@***.$8", $string); // Saída: Meu e-mail é ***@***.com
```

## Veja também

Aqui estão alguns links úteis para aprender mais sobre buscas e substituições em PHP:

- [Documentação do PHP: Função str_replace](https://www.php.net/manual/pt_BR/function.str-replace.php)
- [Documentação do PHP: Função preg_replace](https://www.php.net/manual/pt_BR/function.preg-replace.php)
- [Tutorial sobre expressões regulares em PHP](https://www.tutorialrepublic.com/php-tutorial/php-regular-expressions.php)