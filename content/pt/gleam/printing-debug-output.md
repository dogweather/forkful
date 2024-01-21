---
title:                "Exibindo saídas de depuração"
date:                  2024-01-20T17:52:28.811651-07:00
model:                 gpt-4-1106-preview
simple_title:         "Exibindo saídas de depuração"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?

Imprimir saída de depuração é basicamente escrever informações de diagnóstico no seu console ou logs durante a execução de um programa. Programadores fazem isso para entender o comportamento do código, perseguindo bugs ou certificando-se de que tudo corre como esperado.

## Como Fazer:

No Gleam, você pode usar a função `debug` da biblioteca padrão para imprimir valores enquanto debuga. Aqui está um exemplo:

```gleam
import gleam/io

pub fn main() {
  let x = 42
  io.debug(x)
  x
}
```

Saída de exemplo:
```
42
```

Não se esqueça de remover chamadas de depuração antes de enviar o código!

## Mergulho Profundo

Antigamente, imprimir saída de depuração podia ser tão simples quanto usar `println`. No entanto, com o tempo e o desenvolvimento de sistemas mais complexos, técnicas mais avançadas e ferramentas especializadas, como debuggers e APMs, foram criadas. No Gleam, a função `debug` é uma solução simples e eficaz para a maioria das necessidades de depuração, especialmente quando se está desenvolvendo ou testando. 

Uma alternativa é usar ferramentas de logging com diferentes níveis de severidade, que podem ser configurados para omitir logs de depuração em ambientes de produção. Isso é útil para manter a performance e a limpeza dos logs em produção.

A implementação de saídas de depuração no Gleam é direta e sem surpresas: você passa valores para a função `debug`, e ela os imprime. Simples assim.

## Veja Também

- Um guia para debugging em sistemas Erlang/OTP, os quais o Gleam roda em cima: [https://erlang.org/doc/apps/debugger/debugger_chapter.html](https://erlang.org/doc/apps/debugger/debugger_chapter.html)