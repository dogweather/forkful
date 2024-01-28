---
title:                "Usando um depurador"
date:                  2024-01-26T03:49:21.352875-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando um depurador"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/using-a-debugger.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Usar um depurador é basicamente você atuando como um detetive no seu código, procurando por bugs e tentando entender por que as coisas não estão funcionando bem. Os programadores fazem isso porque, vamos encarar, bugs são inevitáveis, e eliminá-los de maneira eficiente significa colocar seu código para funcionar mais rápido e de forma mais confiável.

## Como fazer:
Atualmente, Gleam se apoia no ecossistema Erlang para ferramentas, então você geralmente depurará com ferramentas como `rebar3`, `observer` e `debugger`. Aqui está como se sujar de verdade com a depuração:

```gleam
// No seu arquivo de configuração do rebar, garanta que você tenha estas linhas para incluir informações de depuração:
{erl_opts, [debug_info]}.

// Execute um shell Erlang com seu app carregado
rebar3 shell

// Dentro do shell, você pode iniciar o depurador
1> debugger:start().
```

Simples, certo? O GUI do `debugger` aparece, e você pode definir pontos de interrupção, avançar pelo código e observar variáveis à vontade. Você não verá o código Gleam diretamente, mas o código Erlang para o qual ele compila, o que ainda é bastante útil.

## Mergulho Profundo
Gleam é uma linguagem jovem, então, enquanto ela se apoia nos ombros do ecossistema Erlang, ferramentas nativas de depuração de Gleam ainda não estão em destaque. Isso significa que estamos utilizando as ferramentas testadas e comprovadas do Erlang, e isso não é uma coisa ruim. O depurador do Erlang está presente desde os anos 90, aperfeiçoado por anos ao eliminar bugs irritantes em sistemas onde a confiabilidade é crucial.

Quanto a alternativas, a rastreabilidade é um método poderoso no mundo BEAM (essa é a máquina virtual que executa o código Erlang e Elixir). Usando `rebar3`, você pode acessar ferramentas como `recon` para rastrear chamadas de função e mergulhar profundamente em questões de desempenho.

A mudança entre escrever Gleam e depurar em Erlang pode parecer como se você estivesse traduzindo seus pensamentos em tempo real. Mas a vantagem é que você dá uma espiada no mundo Erlang, entendendo os blocos de construção do seu aplicativo em sua forma de execução.

## Veja Também
Para expandir seu kit de ferramentas para depuração, confira:

- Documentação do depurador do Erlang: [https://erlang.org/doc/apps/debugger/debugger_chapter.html](https://erlang.org/doc/apps/debugger/debugger_chapter.html)
- A biblioteca `recon` para Erlang: [https://ferd.github.io/recon/](https://ferd.github.io/recon/)
