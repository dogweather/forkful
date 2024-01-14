---
title:                "Rust: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário com Rust?

Muitas vezes, durante o desenvolvimento de um projeto em Rust, pode ser necessário criar um arquivo temporário para armazenar dados temporariamente. Isso pode ser útil para testar uma funcionalidade ou para lidar com grandes conjuntos de dados. Felizmente, Rust torna isso muito fácil de fazer com algumas linhas de código.

## Como criar um arquivo temporário com Rust

A primeira coisa que precisamos fazer é importar a biblioteca "tempfile", que nos permitirá criar um arquivo temporário. Em seguida, podemos usar a função "NamedTempFile::new()" para criar um novo arquivo temporário. Veja o código abaixo:

```Rust
use std::io::prelude::*;
use std::fs::File;
use std::io::Result;

use tempfile::NamedTempFile;
```

Em seguida, podemos escrever dados no arquivo temporário e, finalmente, imprimi-los na tela. Veja o exemplo abaixo:

```Rust
let mut file = NamedTempFile::new()?;
writeln!(file, "Este é um arquivo temporário criado com Rust!")?;
println!("O seguinte conteúdo foi escrito no arquivo temporário:");
println!("{}", &file_path);
```

A saída deve ser semelhante a:

```
O seguinte conteúdo foi escrito no arquivo temporário:
/vari/folders/35/2k1_80cn2537h7bxr5z7l7c40000gn/T/rustlzglhvzor5gpfvo.txt
```

## Detalhando a criação de um arquivo temporário

A função "NamedTempFile::new()" retorna um objeto que implementa a trait "Write", permitindo que escrevamos dados no arquivo temporário usando as funções "writeln!" ou "write!". Além disso, o arquivo é automaticamente apagado quando sua variável sai do escopo.

Também é possível definir um prefixo e/ou sufixo para o nome do arquivo temporário, usando a função "NamedTempFile::with_prefix()" ou "NamedTempFile::with_suffix()". Isso pode ser útil para manter um padrão de nomenclatura em seus arquivos temporários.

## Veja também

- [Documentação da biblioteca "tempfile" em Rust](https://docs.rs/tempfile/3.2.0/tempfile/)
- [Outras maneiras de lidar com arquivos temporários em Rust](https://www.reddit.com/r/rust/comments/c7wd9k/what_is_the_best_way_to_create_a_temporary_file/)