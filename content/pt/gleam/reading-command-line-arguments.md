---
title:    "Gleam: Lendo argumentos da linha de comando"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Por que ler argumentos de linha de comando?

Ler argumentos de linha de comando é uma habilidade importante para qualquer programador, especialmente em Gleam. Com a capacidade de ler e interpretar os argumentos passados pelo usuário, você pode criar programas mais interativos e personalizáveis. Além disso, saber como lidar com argumentos de linha de comando pode facilitar a resolução de problemas e a depuração de erros em seu código.

## Como fazer

Para ler argumentos de linha de comando em Gleam, você pode usar a função `Os.args()`. Esta função retorna uma lista de strings contendo os argumentos passados pelo usuário. Abaixo está um exemplo de como ler e imprimir os argumentos de linha de comando em Gleam:

```Gleam
import Os

pub fn main() {
    let args = Os.args()
    for arg in args {
        io.print(arg)
    }
}
```

Se você executar este código passando alguns argumentos de linha de comando, o resultado será a impressão desses argumentos na tela.

## Mergulho Profundo

Além de simplesmente ler e imprimir os argumentos de linha de comando, é possível realizar outras manipulações com eles. Você pode usar funções como `List.head()` e `List.tail()` para acessar argumentos individuais ou excluí-los da lista, respectivamente. Também é possível converter os argumentos para outros tipos de dados, como números ou booleanos, dependendo das suas necessidades.

## Veja também

- [Documentação Gleam sobre argumentos de linha de comando](https://gleam.run/articles/command-line-arguments)
- [Exemplos de código Gleam no GitHub](https://github.com/gleam-lang)

Fique à vontade para explorar mais sobre argumentos de linha de comando em Gleam e aprimorar suas habilidades de programação!