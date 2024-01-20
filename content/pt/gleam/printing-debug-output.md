---
title:                "Imprimindo saída de debug"
html_title:           "C#: Imprimindo saída de debug"
simple_title:         "Imprimindo saída de debug"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Imprimir saída de depuração é a ação de gerar valores internos de um programa que nos permitem monitorar o seu comportamento. Os programadores fazem isso para identificar, rastrear e corrigir erros ou bugs em um programa.

## Como Fazer:
Aqui está um exemplo simples de como imprimir a saída de depuração no Gleam.

```Gleam
import gleam/io.{println}

fn main() {
  let x = 10
  _ = println("O valor de x é: ", x)
  // Este comentário imprime "O valor de x é: 10" no terminal
}
```
No código acima, estamos usando a função builtin `println` para imprimir no terminal o valor da variável `x`.

## Aprofundando-se
Imprimir a saída de depuração é uma prática comum desde os primórdios da programação. O Gleam, que inicialmente foi projetado para fazer a ponte entre a simplicidade funcional do Erlang e a tipagem estática, fornece funções de depuração semelhantes a outras linguagens de programação com `{println}`.

Alternativas para imprimir a saída de depuração incluem o uso de depuradores e de sondas (probes) dinâmicas.

Quando você utiliza `{println}`, a função envia sua saída para `stdio`, especificamente para o fluxo `stdout`. Em um ambiente de produção, pode ser melhor considerar enviar a saída de depuração para arquivos de log ou para um sistema de logging centralizado.

## Veja Também
Para mais informações sobre depuração no Gleam, visite:
* [Guia Oficial de Gleam](https://gleam.run/book/tour/debugging.html)
* [Documentação do io.println](https://hexdocs.pm/gleam_io/gleam/io/#println)