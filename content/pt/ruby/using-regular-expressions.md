---
title:    "Ruby: Utilizando expressões regulares"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar Expressões Regulares

Expressões Regulares são uma ferramenta útil para realizar busca e manipulação de texto em scripts Ruby. Elas permitem que os programadores encontrem e substituam padrões específicos de texto rapidamente e com precisão. É uma habilidade valiosa a se ter para quem trabalha com processamento de texto ou dados.

## Como usar Expressões Regulares

Para usar Expressões Regulares em Ruby, é necessário utilizar o método `.match` e um objeto de Regexp. Por exemplo:

```Ruby
texto = "Olá, eu sou um texto"
padrao = /Olá/
resultado = texto.match(padrao)
```

Neste exemplo, estamos procurando o padrão "Olá" no texto e armazenando o resultado na variável `resultado`. Podemos então utilizar métodos como `.to_s` para converter o resultado em uma string e imprimir na tela.

```Ruby
puts resultado.to_s
# Output => "Olá"
```

Desta forma, podemos facilmente identificar e extrair partes específicas de um texto usando Regexp.

## Aprofundando nas Expressões Regulares

Além do básico, existem muitos conceitos e funcionalidades avançadas em Expressões Regulares. Por exemplo, é possível utilizar caracteres especiais como `?` e `*` para representar padrões opcionais ou repetidos. Também é possível utilizar classes de caracteres, como `[0-9]`, para encontrar dígitos específicos no texto.

Outra funcionalidade útil é o uso de "grupos de captura", que permitem que apenas parte de um padrão seja extraído do texto. Por exemplo:

```Ruby
data = "Hoje é 28 de Setembro"
padrao = /é (\d{1,2}) de ([a-zA-Z]+)/
resultado = data.match(padrao)
```

Neste caso, ao utilizar o método `[]` em `resultado`, podemos acessar apenas o dia e o mês da data digitada.

```Ruby
puts resultado[1]
# Output => "28"
puts resultado[2]
# Output => "Setembro"
```

Essas são apenas algumas das muitas funcionalidades que podem ser exploradas em Expressões Regulares. É uma ferramenta poderosa e flexível para manipulação de texto em Ruby.

## Veja também
- [Documentação oficial de Expressões Regulares em Ruby](https://ruby-doc.org/core-3.0.1/Regexp.html)
- [Tutorial de Expressões Regulares em Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Livro "Mastering Regular Expressions" (em inglês)](https://www.amazon.com/Mastering-Regular-Expressions-Friedl-3rd/dp/0596528124)