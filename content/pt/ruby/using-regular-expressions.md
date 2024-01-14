---
title:    "Ruby: Usando Expressões Regulares"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Por que utilizar expressões regulares em Ruby?

Expressões regulares são uma ferramenta poderosa de processamento de texto que podem facilitar e agilizar muito o seu código Ruby. Com elas, você pode procurar por padrões específicos em strings e manipulá-los de acordo com suas necessidades. Além disso, uma vez que se torna familiar com as sintaxes das expressões regulares, elas podem ser aplicadas em outras linguagens de programação também.

## Como utilizar expressões regulares em Ruby

Para utilizar expressões regulares em Ruby, você deve utilizar a classe `Regexp` e seu construtor `new`, fornecendo a expressão regular desejada entre duas barras `/`. Vamos ver um exemplo prático:

```Ruby
str = "Olá, eu sou um blog post escrito em Ruby"
regex = /blog post/

str =~ regex
# Retorna a posição inicial da primeira ocorrência da expressão regular na string (8 neste caso)

str[regex]
# Retorna a string que corresponde à expressão regular ("blog post" neste caso)
```

Você também pode utilizar métodos como `match` e `scan` para encontrar todas as ocorrências da sua expressão regular na string, e utilizar as opções `i` (ignore case) e `m` (multiline) para fazer sua busca ser mais flexível. Além disso, é possível utilizar quantificadores como `*` e `+` para indicar respectivamente a presença de 0 ou mais repetições de um padrão e a presença de pelo menos uma repetição de um padrão.

## Uma análise mais aprofundada das expressões regulares em Ruby

Além das funcionalidades mais comuns das expressões regulares, Ruby oferece algumas características extras que tornam seu uso ainda mais poderoso. Por exemplo, você pode utilizar expressões regulares para substituir trechos de strings de acordo com o padrão especificado. Para isso, basta utilizar o método `gsub` e fornecer o novo trecho desejado como segundo argumento. Além disso, é possível nomear grupos de captura em suas expressões regulares para facilitar o processo de manipulação de dados capturados.

## Veja também

- [Documentação da classe Regexp em Ruby](https://docs.ruby-lang.org/en/2.7.0/Regexp.html)
- [Tutorial de expressões regulares em Ruby](https://www.tutorialspoint.com/ruby/ruby_regular_expressions.htm)
- [Guia rápido de expressões regulares em Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)