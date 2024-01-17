---
title:                "Criando um arquivo temporário"
html_title:           "Rust: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# O que e por que criar arquivos temporários?

Criar um arquivo temporário é uma técnica comum usada pelos programadores para armazenar dados temporários de maneira eficiente. Isso permite que o programa armazene e manipule dados que não são necessários permanentemente, economizando espaço e recursos.

# Como fazer:

```rust
use std::fs::File;
use std::io::prelude::*;

// Cria um arquivo temporário e escreve "Olá mundo!" nele
let mut file = File::create("temp.txt")?;
file.write_all("Olá mundo!")?;

// Lê o conteúdo do arquivo e imprime no console
let mut content = String::new();
file.read_to_string(&mut content)?;
println!("{}", content); // "Olá mundo!"

// Apaga o arquivo temporário
std::fs::remove_file("temp.txt")?;
```

# Profundando na técnica:

Criar arquivos temporários é uma prática comum na programação, especialmente em sistemas operacionais Unix, onde é uma parte importante da estrutura de arquivos. As alternativas a essa técnica incluem usar variáveis em memória ou criar novos arquivos permanentes, mas essas soluções podem ser menos eficientes.

Ao criar um arquivo temporário, o sistema operacional o cria com uma nomeação exclusiva, geralmente baseada na data e hora, e o salva na pasta temporária do sistema. Esse arquivo será automaticamente excluído quando o programa terminar de executar, evitando a necessidade de limpeza manual.

# Veja também:

- Documentação oficial do Rust sobre criação de arquivos temporários: https://doc.rust-lang.org/std/fs/struct.File.html
- Tutorial sobre como criar e manipular arquivos temporários em Rust: https://www.tutorialspoint.com/rust/rust_files.htm
- Artigo sobre estruturas de arquivos em sistemas operacionais Unix: https://www.tutorialspoint.com/unix/unix-file-management.htm