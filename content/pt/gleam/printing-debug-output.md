---
title:    "Gleam: Imprimindo saída de depuração"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que

Às vezes, quando estamos codificando em Gleam, precisamos verificar o que está acontecendo em nosso programa. Uma maneira simples de fazer isso é imprimir saídas de depuração no console. Isso pode nos ajudar a entender melhor o fluxo do programa e identificar possíveis erros.

## Como fazer

Imprimir saídas de depuração em Gleam é bastante simples. Podemos usar a função `io.inspect` e passar o valor que desejamos imprimir como argumento. Por exemplo:

```
Gleam.program
module Main

import gleam/io
import gleam/string

pub fn main() {
  let name = "Gleam"
  io.inspect(name)
  io.inspect(string.length(name))
}
```

Isso irá imprimir o valor da variável `name` e seu comprimento no console, que no caso seria "Gleam" e 5, respectivamente.

## Profundando no assunto

Ao imprimir saídas de depuração, também podemos utilizar formatação para tornar a saída mais legível e informativa. Podemos especificar o formato desejado como o segundo argumento da função `io.inspect`, usando a sintaxe `%<format>`. Por exemplo:

```
io.inspect(name, %{"O nome é %{name} e possui %{length} letras"})
```

Isso resultaria na seguinte saída: "O nome é Gleam e possui 5 letras". Dessa forma, podemos ter uma visão mais detalhada do valor que estamos inspecionando.

## Veja também

- Documentação oficial do Gleam sobre o uso de saídas de depuração: https://gleam.run/docs/getting-started/printing-debug-output
- Artigo sobre saídas de depuração em Gleam do blog "Borja's Playground": https://www.borjasalguero.com/learn-gleam-print-debug-output 
- Exemplo de código do uso de saídas de depuração no repositório oficial do Gleam: https://github.com/gleam-lang/gleam/blob/main/examples/debug_output.gleam