---
title:    "Elixir: Busca e substituição de texto"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

##Porque

Ao escrever código em Elixir, muitas vezes é necessário fazer alterações em textos para torná-los mais legíveis ou corrigir erros. A busca e substituição de texto é uma técnica útil para realizar essas mudanças rapidamente e com precisão.

##Como Fazer

Para fazer uma busca e substituição de texto em Elixir, usamos a função `String.replace/4`. Esta função recebe quatro parâmetros: o texto original, o texto a ser substituído, o novo texto e, opcionalmente, uma opção de `global` para substituir todas as ocorrências.

```Elixir
iex> String.replace("Olá, mundo!", "mundo", "Elixir")
"Olá, Elixir!"
```

Você também pode usar expressões regulares para fazer a substituição. No exemplo abaixo, usamos a expressão regular `\d+` para corresponder a qualquer dígito e substituí-lo por `#`.

```Elixir
iex> String.replace("O número de telefone é (123) 456-7890", ~r/\d+/, "#")
"O número de telefone é (#) # #-#"
```

##Mergulho Profundo

A função `String.replace/4` usa uma chamada de função `:unicode.replace/4` internamente para fazer a substituição de texto. Isso significa que a busca e substituição de texto em Elixir é compatível com Unicode e pode lidar com diferentes conjuntos de caracteres.

Além disso, você ainda pode usar certas opções de expressão regular, como `ignorecase` e `extended` ao usar essa função. Isso pode ser útil para casos em que você precisa fazer substituições sem diferenciar entre letras maiúsculas e minúsculas ou quando precisa usar espaços em branco e comentários em suas expressões.

##Veja Também

- Documentação oficial de Elixir sobre `String.replace/4`: https://hexdocs.pm/elixir/String.html#replace/4
- Artigo do site Elixir School sobre expressões regulares: https://elixirschool.com/pt/lessons/advanced/regex/
- Guia de expressões regulares em Elixir da ThoughtBot: https://thoughtbot.com/blog/elixir-regex-tutorial