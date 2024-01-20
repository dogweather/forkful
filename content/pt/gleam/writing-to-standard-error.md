---
title:                "Escrevendo no erro padrão"
html_title:           "Arduino: Escrevendo no erro padrão"
simple_title:         "Escrevendo no erro padrão"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## O Que é e Por Que?
Escrever no erro padrão (stderr) permite separar mensagens de erro e logs do fluxo normal de saída de dados (stdout). Programadores fazem isso para diagnosticar problemas sem interferir na saída esperada de um programa.

## Como Fazer:
```gleam
import gleam/io

pub fn main() {
  io.print("Aqui está a saída normal.")
  io.eprint("Ops! Algo deu errado.")
}
```

Saída esperada:

```
Aqui está a saída normal.
Ops! Algo deu errado.
```

Note que a linha com "Ops! Algo deu errado." é enviada para o erro padrão, e pode ser redirecionada separadamente em um terminal.

## Mergulhando Mais Fundo:
Historicamente, o conceito de stderr remonta aos primeiros dias dos sistemas Unix, onde foi introduzido para ajudar a separar o fluxo de saída normal do output de erro. Alternativas incluem o registro em arquivos e o uso de sistemas de log estruturados; no entanto, escrever para stderr ainda é um método rápido e universal. Na implementação, stderr é um fluxo de saída não-buffered, o que significa que as mensagens são exibidas imediatamente, sem esperar que o buffer fique cheio.

## Ver Também:
- Discussão sobre stdout vs stderr no Unix: [https://unix.stackexchange.com/questions/331611/do-not-understand-what-stdout-stderr-stdin-is](https://unix.stackexchange.com/questions/331611/do-not-understand-what-stdout-stderr-stdin-is)
- Práticas recomendadas de registro de erros para aplicativos de software: [https://12factor.net/logs](https://12factor.net/logs)