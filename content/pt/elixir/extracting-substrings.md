---
title:                "Extraindo substrings"
html_title:           "Bash: Extraindo substrings"
simple_title:         "Extraindo substrings"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Extrair substrings consiste em obter uma série contígua de caracteres de uma string, definida pelo seu ponto inicial e final. Programadores fazem isso para manipular ou analisar partes específicas de strings de forma mais eficiente.

## Como:

Para extrair substrings em Elixir, usamos a função `String.slice/3`. Aqui está um exemplo:

```Elixir
String.slice("Olá, mundo do Elixir!", 0, 5)
# Devolve: "Olá, "
```

E se a posição final for maior que o comprimento da string? Não tem problema, Elixir te cobre:

```Elixir
String.slice("Elixir é incrível!", 0, 50)
# Devolve: "Elixir é incrível!"
```

## Devaneio 

Historicamente, muitas outras linguagens ofereciam métodos semelhantes para extrair substrings. Em Elixir, `String.slice/3` funciona perfeitamente em conjunto com o comportamento de padrões de string Unicode.

Como alternativa, para sequências contínuas, também podemos usar o operador binário. No entanto, lembre-se que isso exigirá um conhecimento aprofundado dos binários e da representação interna de string.

Em termos de detalhes de implementação, o Elixir implementa substrings realmente como fatias. Isso significa que a operação é O(1), em outras palavras, é incrivelmente eficiente!

## Veja Também

A documentação oficial sobre String é um ótimo ponto de partida para se familiarizar com as cordas em Elixir: https://hexdocs.pm/elixir/String.html

Se quiser saber mais sobre como as strings são implementadas em Elixir, este post é um excelente recurso: https://www.joaothallis.com.br/por-dentro-das-strings-em-elixir/ 

Para mais informações sobre trabalhar com Unicode em Elixir, confira esta apresentação detalhada: https://pragtob.wordpress.com/2017/04/18/the-wonderful-world-of-elixir-unicode/