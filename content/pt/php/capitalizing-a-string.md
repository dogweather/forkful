---
title:    "PHP: Capitalizando uma string."
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Por que capitalizar uma string?

Capitalização é o processo de alterar uma string para que a primeira letra de cada palavra seja maiúscula. Embora possa parecer um pequeno detalhe, capitalizar uma string pode ser útil em diversas situações, como melhorar a legibilidade de um texto ou seguir padrões de formatação em projetos de programação.

## Como fazer isso em PHP

Em PHP, capitalizar uma string é um processo simples. Usando a função `ucwords()`, podemos capitalizar a primeira letra de cada palavra dentro de uma string. Veja o exemplo abaixo:

```PHP
$string = "exemplo de uma string";
echo ucwords($string); // Saída: Exemplo De Uma String
```

Podemos também utilizar a função `ucfirst()` para capitalizar apenas a primeira letra da string. Confira o exemplo:

```PHP
$string = "exemplo de uma string";
echo ucfirst($string); // Saída: Exemplo de uma string
```

## Aprofundando no assunto

Além das funções mencionadas acima, existem outras formas de capitalizar uma string em PHP. Por exemplo, podemos usar a função `str_replace()` para substituir a primeira letra de cada palavra por sua letra maiúscula correspondente. Veja um exemplo:

```PHP
$string = "exemplo de uma string";
echo str_replace(" ", "-", ucwords($string)); // Saída: Exemplo-De-Uma-String
```

Outra opção é usar expressões regulares em conjunto com a função `preg_replace_callback()`. Isso nos permite manipular a string de forma mais personalizada. Confira o exemplo abaixo:

```PHP
$string = "exemplo de uma string";
echo preg_replace_callback('/\b(\w)/', function($match) { return strtoupper($match[1]); }, $string); // Saída: Exemplo De Uma String
```

## Veja também

- [Documentação do PHP sobre `ucwords()`](https://www.php.net/manual/pt_BR/function.ucwords.php)
- [Artigo sobre expressões regulares em PHP](https://www.devmedia.com.br/expressoes-regulares-em-php/29076)
- [Vídeo tutorial sobre manipulação de strings em PHP](https://www.youtube.com/watch?v=2PmunO1OwNo)