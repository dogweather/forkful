---
title:                "Interpolando uma string"
html_title:           "Elm: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O que & Porquê?
A interpolação de string é uma técnica usada pelos programadores para criar mensagens personalizadas a partir de uma string base combinada com variáveis ou outros valores. Isso permite que as mensagens sejam dinâmicas e se adaptem a diferentes contextos e entradas. É uma ferramenta útil para tornar as aplicações mais interativas e agradáveis aos usuários.

## Como fazer:
Veja abaixo como realizar a interpolação de string em Elm:

```Elm
nome = "João"
mensagem = "Olá, {nome}! Bem-vindo ao meu site."
```

Neste exemplo, a variável "nome" é inserida na string usando um par de colchetes e o nome da variável dentro deles. A saída esperada será: "Olá, João! Bem-vindo ao meu site."

## Profundidade:
A interpolação de string é uma técnica comum em muitas linguagens de programação modernas, como JavaScript e Python. Também é chamada de "formatação de string" em alguns lugares. Uma alternativa para a interpolação de string é o método "concatenação de string", onde valores e strings são combinados usando o operador "+".

A implementação da interpolação de string em Elm é feita usando o operador "|" e a função "format". A expressão "formato" contém a string base e uma lista de valores a serem inseridos. Para mais informações sobre a sintaxe e possíveis opções de formatação, consulte a documentação oficial.

## Veja também:
- Documentação oficial Elm sobre interpolação de string:
https://guide.elm-lang.org/interop/string.html