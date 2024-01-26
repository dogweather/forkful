---
title:                "Organizando o código em funções"
date:                  2024-01-26T01:10:36.993298-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizando o código em funções"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Organizar o código em funções significa dividir o comportamento de um programa em partes menores e reutilizáveis. Os programadores fazem isso para tornar o código mais claro, mais fácil de manter e para evitar repetição.

## Como Fazer:
Aqui está um exemplo simples de organização de código em funções em Gleam:

```gleam
fn add(x, y) {
  x + y
}

fn main() {
  let sum = add(3, 4)
  sum
}

// Saída de exemplo
// 7
```

Neste trecho, `add` é uma função que pega dois valores e os soma. `main` é onde chamamos `add` e gerenciamos o resultado.

## Aprofundamento
Historicamente, o conceito de funções (ou 'sub-rotinas') revolucionou a programação, abrindo caminho para a programação estruturada nos anos 1960 e além. As funções encorajam uma abordagem modular, onde os problemas são divididos em subproblemas, resolvidos independentemente e compostos para resolver o problema maior.

Em Gleam, que é fortemente tipada, as funções também carregam informações de tipo, garantindo que seu uso seja consistente com sua definição. Isso reduz erros e esclarece intenções.

Alternativas a funções incluem a codificação inline, onde a lógica é repetidamente escrita. Embora, às vezes, mais rápida para tarefas pequenas e únicas, a codificação inline não escala bem para aplicações maiores.

Detalhes de implementação a considerar ao organizar em funções podem incluir composição de funções, onde funções são usadas como blocos de construção, e funções de ordem superior, que recebem outras funções como argumentos ou as retornam, adicionando flexibilidade a como o código é organizado e executado.

## Veja Também
Para mais sobre funções em Gleam, você pode mergulhar na documentação oficial em:
- [Funções da linguagem Gleam](https://gleam.run/book/tour/functions.html)

Ou explorar conceitos de programação mais amplos:
- [Rede de Desenvolvedores Mozilla sobre Funções JavaScript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Guide/Functions)
- [Learn You Some Erlang for Great Good! - Sobre Módulos e Funções](https://learnyousomeerlang.com/modules)