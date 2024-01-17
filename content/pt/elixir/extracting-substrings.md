---
title:                "Extraindo subcadeias"
html_title:           "Elixir: Extraindo subcadeias"
simple_title:         "Extraindo subcadeias"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

##O que e Porque?
Extrair substrings é o processo de obter parte de uma string maior em uma string separada. Programadores frequentemente realizam esta tarefa para tratar strings mais facilmente, trabalhando com apenas a informação necessária em determinado momento.

##Como fazer:
Extrair substrings em Elixir é simples e direto. Usando a função `String.slice`, podemos especificar o índice inicial e o tamanho da substring que queremos obter.

    ```Elixir
      string = "Elixir é uma ótima linguagem de programação"
      String.slice(string, 7, 5) # retorna "é uma"
    ```

Também é possível usar a função `String.substr` para especificar apenas o índice inicial e obter o resto da string a partir desse ponto.

    ```Elixir
      string = "Elixir é uma ótima linguagem de programação"
      String.substr(string, 26) # retorna "linguagem de programação"
    ```

##Mergulho Profundo:
Extrair substrings é uma tarefa essencial em muitas linguagens de programação, pois permite que os programadores trabalhem com strings de maneira mais fácil e eficiente. Outras linguagens, como Java e Python, também possuem funções semelhantes para a extração de substrings.

A implementação da função `String.slice` em Elixir é baseada em uma biblioteca C chamada libstring, que contém várias funções para manipulação de strings.

##Veja Também:
- [Documentação Oficial do Elixir sobre Strings](https://hexdocs.pm/elixir/String.html)
- [Página da Biblioteca libstring](https://github.com/sergey-miryanov/libstring)