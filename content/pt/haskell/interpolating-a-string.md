---
title:                "Interpolando uma string"
html_title:           "Haskell: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O que & por que?

Interpolação de string é o processo de combinar strings com variáveis ou expressões em uma única string. Isso permite que os programadores incluam valores dinâmicos em suas strings, tornando-as mais flexíveis e dinâmicas. É comumente usado em linguagens de programação funcional, como Haskell, para criar mensagens personalizadas ou construir strings complexas em tempo de execução.

## Como fazer:

Usando Haskell, podemos criar strings interpoladas usando a sintaxe ```[variável|expressão]```, onde a variável é substituída pelo seu valor e a expressão é avaliada e incorporada à string. Veja abaixo um exemplo de como criar e imprimir uma string interpolada em Haskell:

```Haskell
idade = 21
nome = "Maria"
print [nome|Olá, meu nome é | e eu tenho |idade| anos.]
```

Saída:
```
Olá, meu nome é Maria e eu tenho 21 anos.
```

## Mergulho Profundo:

A interpolação de string é comumente usada em linguagens de programação funcional como uma alternativa ao uso de concatenação de strings ou formatação de strings. Isso permite que o código seja mais legível e fácil de manter. Além disso, a sintaxe de interpolação de string em Haskell é baseada na ideia de "quasiquoting", uma técnica que permite a manipulação de código fonte em tempo de compilação.

Uma alternativa à interpolação de string em Haskell é o uso de um monad, como o monad de string, que permite que as strings sejam compostas usando operadores especiais, tornando a sintaxe mais semelhante à linguagem natural.

A implementação da interpolação de string em Haskell é baseada no uso de template haskell, uma funcionalidade que permite a manipulação de código fonte em tempo de compilação. Isso permite que os valores das variáveis e expressões sejam avaliados antes da compilação, garantindo um desempenho mais rápido em tempo de execução.

## Ver Também:

Para mais informações sobre interpolação de string em Haskell, você pode consultar a documentação oficial da linguagem ou também pode explorar alguns dos tópicos relacionados, como quasiquotation e template haskell.