---
title:                "PHP: Concatenando strings"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Porque concatenar strings é importante?

A concatenação de strings é uma técnica fundamental na programação em PHP. Ela permite que diferentes strings (ou cadeias de caracteres) sejam unidas em uma única, facilitando a criação de mensagens e conteúdos dinâmicos.

## Como fazer a concatenação de strings em PHP

Para concatenar strings em PHP, podemos utilizar o operador de concatenação: `.` (ponto). Por exemplo:

```PHP
$nome = "João";
$sobrenome = "Silva";
echo "Olá, " . $nome . " " . $sobrenome . "!"; // Saída: Olá, João Silva!
```

Também é possível utilizar a função `concat()` para realizar a concatenação de forma mais eficiente:

```PHP
$nome = "Maria";
$sobrenome = "Fernandes";
$mensagem = concat("Olá,", $nome, $sobrenome, "!", "<br>");
echo $mensagem; // Saída: Olá, Maria Fernandes!<br>
```

Além disso, também é possível concatenar variáveis com strings ou até mesmo com outros tipos de dados, como números:

```PHP
$idade = 32;
$mensagem = "Eu tenho " . $idade . " anos.";
echo $mensagem; // Saída: Eu tenho 32 anos.
```

## Aprofundando na concatenação de strings

É importante lembrar que, ao utilizar a função `concat()`, é possível definir qualquer número de parâmetros para serem concatenados, tornando-a bastante versátil. Além disso, o operador de concatenação também pode ser utilizado em expressões maiores, como arrays e objetos.

Outro ponto importante é a utilização do método `sprintf()` para formatar strings e inserir variáveis em posições específicas. Por exemplo:

```PHP
$nome = "Ana";
$sobrenome = "Souza";
$mensagem = sprintf("Olá, %s %s!", $nome, $sobrenome);
echo $mensagem; // Saída: Olá, Ana Souza!
```

Com essa técnica, também podemos definir o tipo de dado que será exibido, como `%d` para números inteiros e `%f` para números de ponto flutuante.

## Veja também

- [Documentação do PHP sobre Concatenação de Strings](https://www.php.net/manual/pt_BR/language.operators.string.php)
- [Guia de Referência de PHP para Concatenação de Strings](https://www.w3schools.com/php/php_string_concat.asp)
- [Tutorial de PHP sobre concatenação e outras operações básicas com strings](https://www.devmedia.com.br/operacoes-basicas-com-strings-no-php/20845)