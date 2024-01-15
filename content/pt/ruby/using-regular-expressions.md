---
title:                "Utilizando expressões regulares"
html_title:           "Ruby: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar expressões regulares?

As expressões regulares são padrões utilizados para realizar buscas e substituições de strings em um texto. Elas são extremamente úteis para tarefas que envolvem manipulação de dados, validação de inputs e extração de informações específicas de um texto.

## Como utilizar expressões regulares em Ruby?

Para utilizar expressões regulares em Ruby, é necessário usar a classe `Regexp` do módulo `StdLib`. Veja abaixo dois exemplos simples de como utilizar expressões regulares para encontrar padrões em uma string e substituir trechos de texto.

```Ruby
# Encontrando padrões
texto = "Hoje é um belo dia para programar em Ruby!"
puts texto =~ /Ruby/ # o símbolo =~ verifica se o padrão está presente na string, retornando a posição do primeiro caractere encontrado

# Substituindo trechos de texto
texto = "Oi, meu nome é Maria!"
puts texto.sub(/Maria/, "Joana") # a função sub troca o primeiro padrão encontrado pelo segundo argumento
```

Output:
```
32
Oi, meu nome é Joana!
```

## Mergulho Profundo em Expressões Regulares

Existem diversos métodos na classe `Regexp` que podem ser utilizados para realizar operações mais complexas com expressões regulares. Alguns deles são:

- `match`: encontra o primeiro padrão na string e retorna um objeto `MatchData` com informações sobre o padrão encontrado.
- `scan`: encontra todos os padrões na string e retorna um array.
- `split`: divide a string em um array, utilizando o padrão como separador.
- `gsub`: substitui todos os padrões encontrados na string pelo segundo argumento.
- `options`: permite adicionar opções à expressão regular, como ignorar letras maiúsculas ou usar multiplas linhas.

Para saber mais sobre a sintaxe de expressões regulares em Ruby, confira a documentação oficial: [https://ruby-doc.org/core-2.7.1/Regexp.html](https://ruby-doc.org/core-2.7.1/Regexp.html)

## Veja também

Aprenda a usar expressões regulares com esses tutoriais e exercícios interativos:

- [https://www.rubyguides.com/2015/06/ruby-regex/](https://www.rubyguides.com/2015/06/ruby-regex/)
- [https://rubular.com/](https://rubular.com/)