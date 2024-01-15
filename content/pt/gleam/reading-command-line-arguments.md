---
title:                "Lendo argumentos da linha de comando"
html_title:           "Gleam: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Porquê

Ler argumentos de linha de comando pode ser uma tarefa crucial ao desenvolver aplicações ou scripts. Compreender como isso funciona permitirá que você crie programas mais versáteis e automatizados.

## Como Fazer

Ler argumentos de linha de comando é uma habilidade importante ao trabalhar com a linguagem de programação Gleam. Aqui está um exemplo simples de como ler argumentos de linha de comando em um script:

```Gleam
fn main() {
  let args = os.args()
}
```

Este código armazenará todos os argumentos de linha de comando em uma lista chamada `args`, que pode ser usada para executar ações específicas no seu script.

Você também pode ler argumentos individuais, especificando sua posição na lista. Por exemplo, se você quer acessar o primeiro argumento, pode usar `args[0]`.

## Deep Dive

Além de ler argumentos diretamente da linha de comando, a linguagem Gleam também oferece a funcionalidade de passar argumentos para sua aplicação através de variáveis de ambiente. Você pode especificar o valor de uma variável de ambiente diretamente na linha de comando, usando a sintaxe `VAR=valor`. Isso pode ser útil se você precisar passar informações sensíveis, como senhas, que não devem ser explicitamente visíveis no seu código.

Outra vantagem de ler argumentos de linha de comando é que você pode criar programas interativos que permitem que o usuário insira informações em tempo de execução. Isso pode ser útil para jogos, estudos de caso ou aplicações de linha de comando que requerem entrada do usuário.

## Veja Também

- [Documentação oficial da linguagem Gleam](https://gleam.run/documentation)
- [Exemplos de código da linguagem Gleam](https://github.com/gleam-lang/gleam/tree/main/examples)
- [Tutorial da linguagem Gleam na Hackernoon](https://hackernoon.com/a-beautifully-typesafe-language-that-runs-erlang-code-and-compiles-to-javascript-w16g3xei)