---
title:                "Usando um shell interativo (REPL)"
date:                  2024-01-26T04:14:21.732128-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando um shell interativo (REPL)"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## O Quê & Por Quê?

Um REPL, abreviação para Read-Eval-Print Loop (Laço de Ler-Avaliar-Imprimir), é uma ferramenta de programação para executar código de forma interativa e ver resultados instantaneamente. Programadores a usam para experimentar, depurar ou aprender uma nova linguagem em tempo real, como o Gleam.

## Como fazer:

Atualmente, o Gleam não inclui um REPL em sua distribuição padrão. No entanto, você pode experimentar com o código Gleam usando o shell Erlang existente, porque o Gleam compila para o bytecode Erlang. Veja como:

1. Compile seu código Gleam para Erlang.
```plaintext
gleam build
```

2. Inicie o shell Erlang.
```plaintext
erl -pa ebin
```

3. Chame suas funções Gleam (assumindo que você tenha um módulo chamado `my_mod` e uma função `my_fun`).
```erlang
my_mod:my_fun().
```

Você deve ver a saída da sua função exibida no shell.

## Aprofundando

REPL incorpora o espírito dinâmico e exploratório de muitas linguagens de programação funcionais, remontando ao REPL de LISP nos anos 1960. Comparativamente, outros sistemas como `ipython` do Python ou `irb` do Ruby oferecem experiências semelhantes para suas comunidades.

Embora o Gleam ainda não tenha um REPL nativo, tirar proveito do shell Erlang permanece uma solução inteligente. As capacidades do shell Erlang vêm da BEAM VM, a máquina virtual que alimenta o ecossistema Erlang, que inclui Elixir, LFE e Gleam.

Alternativas para REPLs no ecossistema Gleam poderiam incluir a escrita de casos de teste ou o uso de compiladores online e playgrounds de código que suportam Gleam, para testar trechos de código fora de uma configuração de projeto completa.

A implementação de um REPL Gleam dedicado enfrenta desafios principalmente em torno da natureza compilada do Gleam e do tempo de execução do Erlang, onde a troca de código quente é a norma. Qualquer REPL Gleam futuro precisaria reconciliar a tipagem estática da linguagem com o ambiente de execução dinâmico que um REPL espera.

## Veja também

- Documentação oficial do Gleam: https://gleam.run/book/
- Documentação do shell Erlang: http://erlang.org/doc/man/erl.html
- Um playground de compilador Gleam online: https://gleam.run/compiler/