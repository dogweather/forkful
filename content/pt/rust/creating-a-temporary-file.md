---
title:                "Criando um arquivo temporário."
html_title:           "Rust: Criando um arquivo temporário."
simple_title:         "Criando um arquivo temporário."
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que

Às vezes, em nossos programas, precisamos criar arquivos temporários para armazenar dados ou resultados temporários. Criar um arquivo temporário é útil quando não queremos sobrecarregar nosso sistema com arquivos desnecessários e queremos manter nosso código limpo e organizado.

## Como fazer

Para criar um arquivo temporário em Rust, podemos usar a função `std::fs::tempfile()`. Esta função retorna um `std::io::Result` contendo um `File` e o caminho do arquivo recém-criado.

```
Rust
use std::fs::File;
use std::io::prelude::*;

fn main() {
    // Criando um arquivo temporário
    let temp_file = std::fs::tempfile().expect("Erro ao criar arquivo temporário");
    
    // Escrevendo algo no arquivo
    writeln!(temp_file, "Este é um arquivo temporário.").expect("Erro ao escrever no arquivo");
    
    // Lendo o conteúdo do arquivo
    let mut contents = String::new();
    temp_file.read_to_string(&mut contents).expect("Erro ao ler arquivo");
    println!("Conteúdo do arquivo: {}", contents);
    
    // O arquivo será automaticamente deletado ao sair do escopo
}
```

Exemplo de saída:

```
Conteúdo do arquivo: Este é um arquivo temporário.
```

## Profundando mais

Podemos especificar um diretório onde queremos que o arquivo temporário seja criado usando o método `tempfile_in()`, ao invés de deixar o Rust escolher o diretório padrão. Além disso, podemos alterar o nome do arquivo gerado usando o método `tempfile_with()`.

Também é importante lembrar que o arquivo temporário será automaticamente excluído quando sairmos do escopo do arquivo, a menos que decidamos movê-lo para outro local usando o método `tempfile().persist()`.

## Veja também

- Documentação oficial sobre a função `std::fs::tempfile()` em Rust: https://doc.rust-lang.org/std/fs/fn.tempfile.html
- Exemplos de uso da função `std::fs::tempfile()` em Rust: https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=4d87bed22608010a6ca8973ba5ecf573