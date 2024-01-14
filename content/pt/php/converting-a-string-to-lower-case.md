---
title:    "PHP: Convertendo uma string para letras minúsculas"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que converter uma string para letras minúsculas?

Existem diversas razões pelas quais um desenvolvedor PHP pode precisar converter uma string para letras minúsculas. Algumas delas incluem melhorar a legibilidade do código, garantir que o programa funcione corretamente ao lidar com diferentes entradas do usuário e facilitar a comparação de strings.

## Como realizar a conversão em PHP

Para converter uma string para letras minúsculas em PHP, podemos usar a função embutida `strtolower()`. Veja o exemplo abaixo:

```PHP
<?php
$string = "Olá MUNDO";
echo strtolower($string);
```

**Saída:** olá mundo

É importante notar que essa função não altera a string original, mas retorna uma nova string em letras minúsculas.

## Aprofundando na conversão de strings para letras minúsculas

Quando se trata de converter strings para letras minúsculas em PHP, é importante ter em mente a codificação dos caracteres. Em algumas codificações, como UTF-8, existem caracteres que têm letras maiúsculas e minúsculas diferentes, e a conversão pode não funcionar corretamente.

Para lidar com essa situação, é recomendável utilizar a função `mb_strtolower()` em vez da `strtolower()`, pois ela lida melhor com diferentes codificações.

Outro fator importante é o idioma utilizado. Alguns idiomas têm regras específicas de conversão de letras maiúsculas para minúsculas, portanto, é sempre bom verificar a documentação do PHP para garantir que a função utilizada seja adequada para o idioma em questão.

## Veja também

- [Documentação oficial do PHP para a função strtolower()](https://www.php.net/manual/pt_BR/function.strtolower.php)
- [Exemplos de uso da função strtolower()](https://www.php.net/manual/pt_BR/function.strtolower.php#example-5015)
- [Documentação oficial do PHP para a função mb_strtolower()](https://www.php.net/manual/pt_BR/function.mb-strtolower.php)
- [Exemplo de uso da função mb_strtolower() com codificação UTF-8](https://www.php.net/manual/pt_BR/function.mb-strtolower.php#example-5304)