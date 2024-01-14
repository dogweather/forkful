---
title:    "PHP: Apagando caracteres correspondentes a um padrão"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Por que?

Há várias razões pelas quais um programador pode optar por deletar caracteres que correspondem a um padrão específico. Pode ser para limpar dados de entrada, remover caracteres indesejados ou até mesmo manipular strings de forma eficiente.

## Como fazer

Para deletar caracteres seguindo um padrão, podemos utilizar a função `preg_replace()` do PHP. Essa função substitui o texto que corresponde a uma expressão regular por um texto de sua escolha. Vamos ver um exemplo prático:

````PHP

$texto = "Bem-vindo à plataforma de programação! #AprendaPHP";

$padrao = "/@[A-Za-z0-9_]+/";

$resultado = preg_replace($padrao, "", $texto);

echo $resultado; // Resultado: Bem-vindo à plataforma de programação!

````

Neste exemplo, utilizamos a expressão regular `/@[A-Za-z0-9_]+/` para encontrar todos os caracteres que começam com "@" seguido de letras, números ou "_". Em seguida, substituímos esses caracteres vazios, deixando apenas o texto desejado.

## Deep Dive

A função `preg_replace()` pode ser utilizada de diversas formas para se adaptar às necessidades de cada programador. Além disso, é possível utilizar expressões regulares mais complexas para encontrar padrões ainda mais específicos. É importante estudar e compreender bem o conceito de expressões regulares para utilizar essa função de forma eficaz.

## Veja também

- [Documentação oficial do PHP para a função preg_replace](https://www.php.net/manual/en/function.preg-replace.php)
- [Tutorial sobre expressões regulares do PHP](https://www.tutorialrepublic.com/php-tutorial/php-regular-expressions.php)
- [Comunidade brasileira de programadores PHP](https://php.rio/)