---
title:                "Rust: Gerando um arquivo temporário"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em Rust?

Criar um arquivo temporário pode ser útil em diversas situações ao escrever um código em Rust. Algumas razões comuns para criar um arquivo temporário incluem armazenar dados temporários, fazer testes, manipular arquivos grandes ou simplesmente para evitar sobrecarregar o armazenamento permanente.

## Como criar um arquivo temporário em Rust

Para criar um arquivo temporário em Rust, podemos usar a função `tempfile::tempfile()`. Esta função retorna um objeto `Result<File>`, que pode ser usado para escrever ou ler dados do arquivo temporário. Veja um exemplo abaixo:

```Rust
use std::io::prelude::*;
use std::fs::File;
use tempfile::tempfile;

let mut temp_file = tempfile().expect("Não foi possível criar o arquivo temporário");
temp_file.write_all(b"Este é um exemplo de conteúdo do arquivo temporário")
    .expect("Não foi possível escrever dados no arquivo");
```

Neste exemplo, usamos o `tempfile()` para criar o arquivo temporário e em seguida, usamos o método `write_all()` para escrever uma string no arquivo. Podemos também usar métodos como `read()` ou `seek()` para ler e manipular os dados no arquivo temporário.

## Aprofundando no assunto

Existem algumas coisas importantes a se ter em mente ao criar um arquivo temporário em Rust. Primeiro, sempre devemos checar o resultado da função `tempfile()` para garantir que o arquivo foi criado com sucesso. Caso contrário, podemos receber um erro ao tentar escrever ou ler dados no arquivo.

Também é importante lembrar que o arquivo temporário será automáticamente deletado quando o programa terminar de executar, então não precisamos nos preocupar em deletá-lo manualmente. Além disso, podemos especificar a extensão do arquivo temporário usando o método `suffix()` para facilitar o reconhecimento do arquivo.

## Veja também

- [Documentação da função `tempfile()`](https://docs.rs/tempfile/3.2.0/tempfile/fn.tempfile.html)
- [Exemplo de uso da biblioteca `tempfile`](https://gist.github.com/LuisHenrique01/00beee1099ca6e3fbea0de3d3b75a97f)
- [Tutorial sobre manipulação de arquivos em Rust](https://doc.rust-lang.org/rust-by-example/std_misc/file/read_lines.html)