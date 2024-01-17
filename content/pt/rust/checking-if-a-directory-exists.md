---
title:                "Verificando se um diretório existe"
html_title:           "Rust: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O que é e por que verificar se um diretório existe?

Verificar se um diretório existe é um processo comum durante a programação em Rust. Isso significa verificar se há um diretório específico em um caminho de diretório especificado. Os programadores fazem isso para garantir que o programa possa acessar ou criar arquivos no diretório especificado.

## Como fazer:

Um exemplo simples de como verificar se um diretório existe em Rust:

```Rust
use std::fs;

if fs::metadata("caminho/do/diretório").is_ok() {
    println!("O diretório existe!");
} else {
    println!("O diretório não existe!");
}
```

Caso o diretório exista, a saída será "O diretório existe!". Caso contrário, a saída será "O diretório não existe!".

## Mergulho aprofundado:

Verificar se um diretório existe é uma tarefa importante para garantir que um programa possa funcionar corretamente. Isso é especialmente importante em sistemas operacionais como o Windows, onde diferentes usuários podem ter permissões de acesso diferentes para diferentes diretórios.

Outra alternativa para verificar se um diretório existe é usando a biblioteca "PathBuf" em Rust. Esta biblioteca possui uma função "exists" que retorna um booleano indicando se o diretório existe ou não.

A implementação para verificar se um diretório existe em Rust usa a função "metadata" da biblioteca "fs". Esta função retorna informações sobre o arquivo ou diretório especificado, incluindo se ele existe ou não.

## Veja também:

1. [Documentação oficial do Rust sobre a função metadata](https://doc.rust-lang.org/std/fs/fn.metadata.html)
2. [Exemplo de código em Rust para verificar se um diretório existe](https://www.tutorialspoint.com/checking-if-a-directory-exists-in-rust)