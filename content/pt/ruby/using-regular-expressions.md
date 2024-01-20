---
title:                "Utilizando expressões regulares"
html_title:           "Bash: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O Que é & Por Que Usar?
Expressões regulares são padrões usados para encontrar uma determinada combinação de caracteres dentro de uma string. Programadores utilizam-nas para validar, extrair ou substituir essas sequências de forma flexível e eficiente em diversas situações.

## Como Fazer:
```Ruby
texto = "Meu email é contato@exemplo.com.br"

# Encontrar o padrão de e-mail
email_regex = /\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,}\b/i
puts texto.match(email_regex)
```
Output:
```
contato@exemplo.com.br
```

```Ruby
frase = "Ruby é incrível! Ruby é divertido!"

# Substituir todas as ocorrências de 'Ruby' por 'Python'
puts frase.gsub(/Ruby/, 'Python')
```
Output:
```
Python é incrível! Python é divertido!
```

## Mergulho Profundo
Expressões regulares foram introduzidas na década de 1950 pelo matemático americano Stephen Kleene. Existem alternativas para manipulação de strings, como métodos de pesquisa e substituição embutidos, mas eles não oferecem a mesma flexibilidade. No Ruby, as expressões regulares são implementadas através do padrão da biblioteca Onigmo (uma evolução do Oniguruma), permitindo uma ampla gama de funcionalidades.

## Veja Também
- [Documentação oficial do Ruby sobre expressões regulares](https://ruby-doc.org/core-2.7.0/Regexp.html)
- [Rubular: um editor de expressões regulares em Ruby](http://rubular.com/)
- [Livro "Expressões Regulares Cookbook" por Jan Goyvaerts e Steven Levithan](https://www.oreilly.com/library/view/regular-expressions-cookbook/9781449327453/)