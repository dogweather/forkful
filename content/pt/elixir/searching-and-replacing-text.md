---
title:                "Busca e substituição de texto"
html_title:           "Elixir: Busca e substituição de texto"
simple_title:         "Busca e substituição de texto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que

Se você é um programador que trabalha com texto, provavelmente já teve que lidar com situações em que precisa procurar e substituir palavras ou trechos específicos. Isso pode ser tedioso e demorado se você estiver fazendo manualmente. Felizmente, com a linguagem de programação Elixir, é possível automatizar esse processo com eficiência e rapidez.

## Como fazer

Para realizar a busca e substituição de texto em Elixir, utilizamos a função `String.replace/4`. Ela recebe quatro parâmetros: uma string alvo, um padrão a ser procurado, a string de substituição e um conjunto de opções. Por exemplo:

```
Elixir> String.replace("Olá mundo!", "mundo", "Elixir")
"Olá Elixir!"
```

Podemos também utilizar regex para procurar um padrão mais complexo. No exemplo abaixo, utilizamos `~r/` e `/` para delimitar o padrão e `i` para indicar que a busca deve ser case insensitive:

```
Elixir> String.replace("Eu amo Elixir!", ~r/am[oa]/i, "adoro")
"Eu adoro Elixir!"
```

## Profundando

Além da função `String.replace/4`, Elixir também possui outras funções úteis para manipulação de strings, como `String.replace_at/3`, `String.replace_prefix/3` e `String.replace_suffix/3`. É importante lembrar que essas funções retornam uma nova string modificada e não alteram a string original.

Outra aplicação útil da busca e substituição de texto é em arquivos. Utilizando a biblioteca `File` e a função `File.write/3`, podemos ler um arquivo, procurar e substituir o padrão desejado e escrever uma nova versão do arquivo com as mudanças feitas.

## Veja também

Para mais informações sobre manipulação de strings em Elixir, confira a documentação oficial: [https://hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)

Para aprender mais sobre programação com Elixir, veja estes recursos recomendados:

- [Site oficial do Elixir](https://elixir-lang.org/)
- [Documentação oficial do Elixir](https://hexdocs.pm/elixir/Kernel.html)
- [Elixir School (em português)](https://elixirschool.com/pt/)