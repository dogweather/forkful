---
title:                "PHP: Utilizando expressões regulares"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar expressões regulares?

Expressões regulares são um recurso muito poderoso no mundo da programação, especialmente em PHP. Elas são úteis na busca, validação e manipulação de strings complexas. Ao dominar as expressões regulares, você pode economizar muito tempo e criar códigos mais eficientes.

## Como usar expressões regulares em PHP

Usar expressões regulares em PHP é simples e fácil. Primeiro, você precisa definir o padrão que deseja encontrar ou validar. Isso pode ser feito usando funções como `preg_match()` ou `preg_replace()`. Em seguida, você deve delimitar o padrão com barras (/) e definir os metacaracteres desejados. Por exemplo:

```PHP
$string = "Olá, meu nome é João.";
$padrao = "/João/"; // Procura pela ocorrência da palavra "João" na string
preg_match($padrao, $string, $ocorrencia);
echo $ocorrencia[0]; // Saída: João
```

Você também pode usar metacaracteres especiais como ponto (.) para representar qualquer caractere, asterisco (*) para representar 0 ou mais repetições e ponto de interrogação (?) para representar 0 ou 1 repetições. Por exemplo:

```PHP
$string = "Bem-vindo ao meu site!";
$padrao = "/site?/"; // Procura por "site" ou "site"
preg_match($padrao, $string, $ocorrencia);
echo $ocorrencia[0]; // Saída: site
```

## Aprofundando nas expressões regulares

Expressões regulares podem ser utilizadas para busca e substituição de strings, validação de formulários, formatação de dados, entre outras tarefas. Além disso, é possível criar padrões mais complexos com uso de quantificadores (como + e {}) e classes de caracteres (como [a-z] e [0-9]).

É importante lembrar que cada linguagem de programação tem sua própria implementação de expressões regulares. Portanto, antes de começar a usá-las em PHP, é importante ler a documentação e praticar para se familiarizar com os padrões e métodos disponíveis.

## Veja também

- [Documentação oficial PHP sobre expressões regulares](https://www.php.net/manual/pt_BR/reference.pcre.pattern.syntax.php)
- [Tutorial de expressões regulares para iniciantes](https://www.tutorialspoint.com/php/php_regular_expression.htm)
- [Ferramenta online para testar expressões regulares](https://regex101.com/)