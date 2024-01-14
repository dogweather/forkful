---
title:    "Gleam: Escrevendo no erro padrão"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Porque

Escrever para o erro padrão, ou standard error, pode ser útil em situações em que precisamos registrar informações de erro em um programa. Isso pode ser especialmente importante em ambientes de produção, onde é essencial capturar e salvar esses erros para fins de depuração.

## Como Fazer

Para escrever para o erro padrão em um programa Gleam, podemos usar a função `erlang:error/1`. Veja um exemplo abaixo:

```Gleam
import gleam/io

fn main() {
  erlang:error("Erro! Esta é uma mensagem de erro.")
}
```

O programa acima irá imprimir a mensagem de erro no erro padrão. Veja o resultado abaixo:

```
Erro! Esta é uma mensagem de erro. 
```

## Mergulho Profundo

Ao escrever para o erro padrão, é importante notar que isto não encerra o programa. Para isso, podemos usar a função `erlang:halt/1`. Além disso, podemos capturar e lidar com erros usando blocos `try/catch`. É recomendado também adicionar informações específicas sobre o erro na mensagem para ajudar na resolução de problemas.

Veja um exemplo abaixo:

```Gleam
import gleam/io
import erlang/try

fn main() {
  x = try sadd(2, "5") catch error -> "Ocorreu um erro ao somar 2 e 5: " ++ getMessage(error)
  io.print(x)
}

fn sadd(x, y) {
  x + y
}
```

E a saída seria:

```
Ocorreu um erro ao somar 2 e 5: bad argument in "2+5"
```

## Veja Também

- [Documentação oficial do Gleam](https://gleam.run/)
- [Artigo sobre a função `erlang:error/1` (em inglês)](https://medium.com/@stoichkovictoramarc/debugging-in-gleam-the-saga-of-erlang-error-1-a0e75b18896f)
- [Exemplo de como lidar com erros em programas Gleam (em inglês)](https://medium.com/@ajpotato/error-handling-in-gleam-a262bba4433d)