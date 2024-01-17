---
title:                "Interpolando uma string"
html_title:           "PHP: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O que e por que?

Interpolar uma string (também conhecida como interpolação de string) é uma técnica comum utilizada por programadores PHP para criar uma nova string combinando variáveis e textos. Isso permite que os programadores criem strings dinâmicas, alterando-as a partir de variáveis definidas no código.

## Como fazer:

```
<?php
$nome = "Mariana";
echo "Olá, $nome! Como vai?";
```

Resultado:
```
Olá, Mariana! Como vai?
```

Neste exemplo, a variável "nome" é interpolada na string, gerando uma saída personalizada.

## Mergulho profundo:

Essa técnica é comumente usada em linguagens de programação e remonta aos primórdios do PHP. Alternativas para interpolar uma string incluem o uso de concatenação de strings com o operador ".", o uso da função "sprintf()" ou a utilização de templates. É importante ter cuidado ao interpolar strings com conteúdo de fontes externas, pois isso pode resultar em vulnerabilidades de segurança.

## Veja também:

- [Documentação oficial do PHP sobre interpolação de strings](https://www.php.net/manual/pt_BR/language.types.string.php#language.types.string.syntax.double)
- [Exemplos práticos de interpolação de strings em PHP](https://www.php.net/manual/pt_BR/language.types.string.php#language.types.string.syntax.double)
- [Tutorial sobre formatação de strings com a função sprintf()](https://www.php.net/manual/pt_BR/function.sprintf.php)