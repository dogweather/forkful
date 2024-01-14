---
title:                "PHP: Excluir caracteres que correspondem a um padrão"
simple_title:         "Excluir caracteres que correspondem a um padrão"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que deletar caracteres que correspondem a um padrão?

Existe uma variedade de situações em que pode ser necessário excluir caracteres que correspondem a um padrão em um código PHP. Isso pode ser útil para simplificar a entrada de dados do usuário, limpar strings de texto ou até mesmo garantir a segurança da aplicação.

## Como fazer:

Para excluir caracteres que correspondem a um padrão em um código PHP, podemos usar a função `preg_replace()` do PHP. Esta função aceita três parâmetros: o padrão a ser buscado, o texto de substituição e a string original. Aqui está um exemplo de código mostrando como usá-la:

```PHP
<?php
	$string = "Olá#mundo!";
	$nova_string = preg_replace('/[#]/', '', $string);
	echo $nova_string; // Saída: Olá mundo!
?>
```

Neste exemplo, usamos o padrão `/[#]/`, que indica que queremos excluir qualquer caractere que corresponda ao símbolo `#`. Para excluir caracteres especiais como o `é` ou `á`, podemos usar o padrão `/[^\pL\pN\s]/`, que exclui todos os caracteres que não são letras, números ou espaços em branco.

## Deep Dive:

Quando se trata de excluir caracteres que correspondem a um padrão mais complexo, podemos usar expressões regulares (regex) para realizar a tarefa. Expressões regulares são padrões de texto que nos permitem encontrar e manipular informações específicas em uma string. Por exemplo, se quisermos excluir todos os números em uma string, podemos usar a expressão regular `/\d+/`, onde `\d+` corresponde a um ou mais dígitos.

## Veja também:

- [Documentação oficial sobre expressões regulares no PHP](https://www.php.net/manual/pt_BR/reference.pcre.pattern.syntax.php)
- [Tutorial sobre expressões regulares no PHP](http://blog.thiagobelem.net/aprendendo-expressoes-regulares/)
- [Ferramentas online para testar expressões regulares](https://regex101.com/)