---
title:                "Ruby: Utilizando Expressões Regulares"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar Expressões Regulares em Ruby

Se você é um programador Ruby, provavelmente já ouviu falar sobre expressões regulares. Mas por que alguém iria querer usá-las? Em resumo, as expressões regulares permitem que você faça buscas e manipulações de strings de forma rápida e eficiente. Com elas, você pode extrair padrões específicos de texto, verificar a validade de dados de entrada e muito mais. Se você deseja aprender mais sobre como usar as expressões regulares em seus projetos Ruby, continue lendo.

## Como usar Expressões Regulares em Ruby

Usar expressões regulares em Ruby é bastante fácil. Primeiro, você precisará criar um objeto RegExp, que pode ser feito adicionando "//" em torno do padrão que você deseja encontrar. Por exemplo:

```Ruby
regex = /amor/
```

Em seguida, você pode usar o método `match` para procurar o padrão em uma string. Veja abaixo um exemplo de busca bem-sucedida:

```Ruby
"Eu amo Ruby!".match(regex)
```

A saída seria:

```Ruby
#<MatchData "amor">
```

Se o padrão não for encontrado, o método `match` retornará `nil`.

Outro método muito útil é o `scan`, que retorna uma matriz com todas as correspondências encontradas em uma string. Veja um exemplo abaixo:

```Ruby
"Amor é tudo que precisamos".scan(regex)
```

A saída seria:

```Ruby
["amor"]
```

Você também pode usar expressões regulares para substituir partes de uma string. O método `sub` substitui a primeira ocorrência do padrão, enquanto o método `gsub` substitui todas as ocorrências. Veja um exemplo abaixo:

```Ruby
"amor é tudo que precisamos".sub(regex, "ódio")
```

A saída seria:

```Ruby
"ódio é tudo que precisamos"
```

Para substituir todas as ocorrências, basta usar o método `gsub`:

```Ruby
"amor é tudo que precisamos".gsub(regex, "ódio")
```

A saída seria:

```Ruby
"ódio é tudo que precisamos"
```

## Mergulho Profundo: Usando Expressões Regulares com Modificadores

Além dos recursos básicos apresentados acima, as expressões regulares em Ruby também suportam modificadores que permitem fazer buscas mais específicas. Alguns dos modificadores mais comuns são `i` (insensitive), para buscar sem distinguir maiúsculas e minúsculas, e `m` (multiline), para fazer uma busca em várias linhas.

Veja abaixo um exemplo de como usar esses modificadores:

```Ruby
regex = /expressão regular/i
texto = "Expressão Regular é uma ferramenta poderosa"
texto.match(regex)
```

A saída seria:

```Ruby
#<MatchData "Expressão Regular">
```

Você também pode combinar vários modificadores em uma mesma expressão regular. Por exemplo, `/amor/iom` buscaria o padrão "amor" sem distinguir maiúsculas e minúsculas, em várias linhas e permitindo que o ponto final `.` corresponda a quebras de linha.

## Veja Também

Aqui estão alguns recursos adicionais para você continuar aprendendo sobre expressões regulares em Ruby:

- [Documentação oficial do Ruby sobre expressões regulares](https://ruby-doc.org/core-3.0.0/Regexp.html)
- [Tutorial de expressões regulares em Ruby](https://www.rubyguides.com/2015/06/ruby-regex/) em RubyGuides
- [Curso de Expressões Regulares no Ruby](https://www.codecademy.com/learn/learn-ruby/pages/regular-expressions-ruby) no Codecademy