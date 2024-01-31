---
title:                "Interpolando uma string"
date:                  2024-01-20T17:50:34.812343-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolando uma string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Interpolar uma string significa enfiar valores de variáveis dentro de uma string, criando assim uma nova string 'personalizada'. Programadores fazem isso para construir mensagens dinâmicas ou para simplificar a concatenação de strings e valores.

## Como Fazer:

Vamos lá, em Elm não temos a tradicional interpolação presente em outras linguagens. Em vez disso, usamos o bom e velho `++` para juntar pedaços de texto. Veja só:

```Elm
nome = "Mundo"
saudacao = "Olá, " ++ nome ++ "!"

-- Saída é: Olá, Mundo!
```

Ah, já ia esquecendo, se você precisar incluir números, converta para string primeiro:

```Elm
idade = 25
mensagem = "Eu tenho " ++ String.fromInt(idade) ++ " anos."

-- Saída é: Eu tenho 25 anos.
```

## Mergulho Profundo:

Antigamente, interpolar strings era um negócio bem manual, como você vê por aqui. Com o passar do tempo, linguagens como JavaScript e Python simplificaram o processo com template strings ou f-strings. Mas Elm gosta de manter as coisas simples e seguras, então continuamos usando a boa concatenação.

Algumas alternativas modernas em Elm são as funções de alto nível como `String.concat` ou combinadores de bibliotecas comunitárias que proporcionam uma sintaxe um pouco mais próxima à interpolação verdadeira.

Sobre a implementação, a concatenação em Elm é eficiente pois a linguagem é otimizada para operações com strings, embora não seja tão chamativa quanto outras formas de interpolação. Mas lembre: em Elm, priorizamos clareza e simplicidade sobre açúcar sintático.

## Veja Também:

Para dar uma aprofundada, confira estes links:

- [Documentação Oficial do Elm sobre Strings](https://package.elm-lang.org/packages/elm/core/latest/String)
- Elm Community String Utils para algumas funções úteis: [elm-community/string-extra](https://package.elm-lang.org/packages/elm-community/string-extra/latest/)
- Artigos e tutoriais mais gerais sobre Elm e suas práticas: [Elm Programming](https://elmprogramming.com/)
