---
title:                "Concatenando strings"
html_title:           "Elixir: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Porque

Você já se perguntou como os aplicativos e sites conseguem combinar diferentes palavras e frases para criar mensagens personalizadas? A resposta é a concatenação de strings, um recurso fundamental na programação de linguagens como o Elixir.

## Como Fazer

Felizmente, o Elixir torna a concatenação de strings extremamente simples e intuitiva. Tudo o que você precisa fazer é usar o operador `<>` para unir diferentes strings juntas.

```
Elixir

nome = "João"
sobrenome = "Silva"
mensagem = "Olá " <> nome <> " " <> sobrenome <> ", seja bem-vindo!"

IO.puts mensagem
```

Isso resultará na saída:

```
Olá João Silva, seja bem-vindo!
```

Você também pode usar variáveis e valores para criar suas strings concatenadas:

```
Elixir

dia = 10
mes = "Agosto"
ano = 2021
mensagem = "Hoje é dia " <> Integer.to_string(dia) <> " de " <> mes <> " de " <> Integer.to_string(ano)

IO.puts mensagem
```

A saída será:

```
Hoje é dia 10 de Agosto de 2021
```

## Mergulho Profundo

Ao concatenar strings, é importante lembrar de prestar atenção nas aspas e espaços. Se você esquecer de adicionar aspas entre suas strings, elas serão tratadas como variáveis e você receberá um erro. Além disso, os espaços precisam ser adicionados manualmente para criar a formatação desejada nas suas mensagens concatenadas.

Outra coisa a se ter em mente é que a concatenação de strings pode afetar a performance do seu código, especialmente se você estiver usando um grande número de strings. Nesses casos, é recomendado o uso da função `Enum.concat` que permite unir um grande número de strings de forma mais eficiente.

## Veja Também

Aqui estão algumas referências adicionais para ajudá-lo a aprimorar suas habilidades em concatenação de strings em Elixir:

- [Documentação do Elixir](https://hexdocs.pm/elixir/String.html#concatenation/2)
- [Tutorial da Elixir School sobre strings](https://elixirschool.com/pt/lessons/basics/strings/)
- [Fórum da Elixir no Reddit](https://www.reddit.com/r/elixir/)

Agora que você dominou a concatenação de strings em Elixir, você pode criar mensagens personalizadas e interativas em seus aplicativos e sites. Divirta-se codando!