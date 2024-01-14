---
title:                "Ruby: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que?

Há momentos em que você precisa fazer alterações em todo o texto de um arquivo, seja para corrigir erros ortográficos ou para substituir uma expressão por outra. Fazer isso manualmente pode ser tedioso e demorado, mas felizmente, existem maneiras de automatizar esse processo através da programação.

## Como Fazer

Em Ruby, podemos usar o método `.gsub()` para realizar a busca e substituição de texto. Este método aceita dois argumentos: o primeiro é a expressão que desejamos substituir e o segundo é a nova expressão que será usada no lugar. Vamos dar uma olhada em um exemplo:

```Ruby
texto = "Estou aprendendo Ruby programação!"
novo_texto = texto.gsub("aprendendo", "explorando")
puts novo_texto
```

O código acima irá resultar em "Estou explorando Ruby programação!". Podemos ver que a palavra "aprendendo" foi substituída por "explorando". Mas e se quisermos substituir todas as ocorrências de uma palavra em uma string? Podemos usar uma expressão regular dentro do método `.gsub()` para isso. Por exemplo:

```Ruby
texto = "A vida é muito curta para aprender uma única linguagem de programação."
novo_texto = texto.gsub(/aprender/, "explorar")
puts novo_texto
```

Agora, todas as ocorrências da palavra "aprender" serão substituídas por "explorar". Além disso, podemos usar a flag `i` para tornar a substituição case-insensitive, ou seja, ela irá substituir tanto "aprender" quanto "Aprender". Assim:

```Ruby
novo_texto = texto.gsub(/aprender/i, "explorar")
```

## Mergulho Profundo

Além do método `.gsub()`, Ruby também possui outros métodos que nos permitem buscar e substituir texto, como o `.sub()`, que substitui apenas a primeira ocorrência encontrada, e o `.scan()`, que nos permite procurar por padrões e retornar uma lista com os resultados encontrados. Existem também gemas (ou gems) como o `Stringex` que fornecem ainda mais funcionalidades para tarefas de busca e substituição de texto.

## Veja Também

- [Documentação do método `.gsub()` em Ruby](https://ruby-doc.org/core-2.5.1/String.html#method-i-gsub)
- [Como usar Expressões Regulares em Ruby](https://www.regexpedia.com/ruby)
- [Gema Stringex para manipulação de strings em Ruby](https://github.com/svenfuchs/stringex)