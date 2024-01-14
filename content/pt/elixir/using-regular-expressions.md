---
title:    "Elixir: Utilizando expressões regulares"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Por que usar expressões regulares em Elixir? 

Expressões regulares são uma ferramenta poderosa e versátil para manipulação de texto. Em Elixir, elas são uma parte importante da linguagem e podem ser usadas para buscar, substituir e validar padrões em strings. Usar expressões regulares em seus programas pode tornar o seu código mais conciso e eficiente, permitindo que você realize tarefas complexas de forma mais simples.

## Como usar expressões regulares em Elixir

Para utilizar expressões regulares em Elixir, é necessário utilizar o módulo `Regex`. Você pode iniciar uma expressão regular utilizando `Regex.new/1` e passando o padrão desejado como parâmetro, dentro de aspas duplas. Por exemplo:

```Elixir
regex = Regex.new("hello")
"hello world" |> Regex.match?(regex) # true
"goodbye" |> Regex.match?(regex) # false
```

O código acima cria uma expressão regular que irá buscar pelo padrão "hello" em uma string. Em seguida, utilizamos a função `match?/2` de `Regex` para verificar se a string fornecida corresponde ao padrão definido.

Você também pode utilizar grupos de captura em suas expressões regulares para extrair informações específicas de uma string. Basta utilizar parênteses para indicar os grupos e depois acessá-los utilizando `Regex.named_captures/1`. Veja um exemplo:

```Elixir
regex = Regex.new("My name is (.*).")
"My name is John." |> Regex.named_captures(regex) # %{"John" => ["John"]}
```

Nesse caso, temos um grupo de captura que irá extrair o nome após a palavra "is". Para acessá-lo, usamos a função `named_captures/1` e passamos a expressão regular como argumento.

## Aprofundando-se nas expressões regulares

As expressões regulares em Elixir são baseadas na sintaxe PCRE (Perl Compatible Regular Expressions). Isso significa que elas compartilham muitas características com expressões regulares em outras linguagens, como Perl, Python e Ruby. Algumas funcionalidades que podem ser utilizadas em Elixir incluem:

- Quantificadores: permitem especificar quantas vezes um padrão deve ser repetido, como `*` (zero ou mais vezes), `+` (uma ou mais vezes) e `?` (zero ou uma vez).
- Classes de caracteres: permitem especificar conjuntos de caracteres que podem ser encontrados em uma determinada posição na string, como `[a-z]` (qualquer letra minúscula) ou `[0-9]` (qualquer dígito).
- Âncoras: permitem especificar posições específicas na string, como `^` (início da linha) e `$` (fim da linha).

Para mais informações sobre o uso de expressões regulares em Elixir, consulte a documentação oficial da linguagem.

# Veja também

- [Documentação oficial do Elixir sobre expressões regulares](https://hexdocs.pm/elixir/Regex.html)
- [Tutorial sobre expressões regulares em Elixir](https://elixirschool.com/en/lessons/advanced/pattern-matching/#regular-expressions)