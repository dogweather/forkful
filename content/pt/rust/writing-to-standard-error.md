---
title:                "Escrevendo para o erro padrão"
html_title:           "Rust: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

O que é e por que escrever para o erro padrão:
Ao programar em Rust, às vezes você vai querer escrever mensagens de erro para o console. Isso é feito escrevendo para o erro padrão, que é uma saída diferente da saída padrão do programa. Os programadores fazem isso para permitir que os usuários vejam mensagens de erro específicas e compreensíveis, em vez de erros genéricos.

Como fazer:
```
// Exemplo de código para escrever em erro padrão
use std::io::Write;

fn main() {
    let mut stderr = std::io::stderr();
    writeln!(stderr, "Este é um erro de exemplo.").unwrap();
}
```

Saída de exemplo:
```
Este é um erro de exemplo.
```

Mergulho profundo:
Para entender melhor por que escrever para o erro padrão é útil, é importante saber que esse recurso existe desde os primeiros sistemas Unix. Na verdade, foi introduzido por Ken Thompson, um dos pioneiros do Unix. Alternativas ao erro padrão incluem escrever para um arquivo de log ou para a saída padrão e redirecioná-la para o console. A implementação do erro padrão em Rust é baseada em um trait chamado "std::io::Write" e é acessível por meio do módulo "std:io".

Veja também:
Para mais informações sobre escrever para o erro padrão em Rust, veja a seção sobre "std::io::Write" na documentação oficial do Rust: https://doc.rust-lang.org/std/io/trait.Write.html