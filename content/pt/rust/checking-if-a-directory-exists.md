---
title:                "Verificando se um diretório existe"
html_title:           "Javascript: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Verificar a existência de um diretório em Rust 

## O Que e Por Quê?
Verificar a existência de um diretório é o processo de determinar se um diretório específico existe ou não. Os programadores fazem isso para evitar erros ao tentar acessar diretórios que podem não existir.

## Como Fazer:
Rust simplifica a verificação da existência de um diretório. Veja o exemplo de código abaixo:

```Rust
use std::path::Path;

fn main() {
    let dir = Path::new("/caminho/para/o/diretório");

    if dir.exists() {
        println!("O diretório existe.");
    } else {
        println!("O diretório não existe.");
    }
}
```

Ao ajustar "/caminho/para/o/diretório" para um caminho de diretório válido, esse programa irá dizer-lhe se o diretório existe ou não pela saída impressa.

## Mergulhando Mais Fundo
A metodologia para verificar a existência de um diretório em Rust é recente no contexto histórico da programação. Alternativamente, outras linguagens de programação, como Python ou Java, têm métodos semelhantes embutidos na linguagem.

O método "exists()" retorna um booleano verdadeiro se o Path referir-se a um diretório existente. Sob o capô, essa função usa a "metadata()" da biblioteca de sistema padrão, que retorna os metadados para um determinado diretório. Se este falhar, o diretório não existe e retorna falso.

## Veja Também:
Existem muitos recursos úteis sobre este tópico que você pode querer verificar. Aqui estão alguns links úteis:

1. Documentação oficial do Rust sobre o método 'exists()': [https://doc.rust-lang.org/std/path/struct.Path.html#method.exists](https://doc.rust-lang.org/std/path/struct.Path.html#method.exists)
3. Para informações mais profundas sobre a implementação do Rust Path and metadata(), confira: [https://doc.rust-lang.org/std/fs/struct.Metadata.html](https://doc.rust-lang.org/std/fs/struct.Metadata.html)