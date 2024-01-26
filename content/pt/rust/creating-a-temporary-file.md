---
title:                "Criando um arquivo temporário"
date:                  2024-01-20T17:41:21.936140-07:00
model:                 gpt-4-1106-preview
simple_title:         "Criando um arquivo temporário"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Criar um arquivo temporário é o processo de gerar um arquivo que é projetado para ser utilizado durante a execução de um programa e, normalmente, apagado quando ele termina. Programadores fazem isso para gerenciar dados temporários, como cache de informações, ou para testes sem afetar arquivos permanentes.

## Como Fazer:

```rust
use std::fs::File;
use std::io::{Write, Seek, SeekFrom};
use tempfile::{tempfile, NamedTempFile};

fn main() -> std::io::Result<()> {
    // Criando um arquivo temporário anônimo
    let mut temp_file = tempfile()?;
    writeln!(temp_file, "Olá, arquivo temporário!")?;
    
    // Criando um arquivo temporário nomeado
    let mut named_temp_file = NamedTempFile::new()?;
    writeln!(named_temp_file, "Este é um arquivo temporário com nome.")?;
    println!("Arquivo temporário salvo em: {:?}", named_temp_file.path());
    
    // Lembre-se, os arquivos temporários são apagados ao sair do escopo
    Ok(())
}
```
_Saída esperada:_
```
Arquivo temporário salvo em: "/tmp/.tmpXXXXX"  // O caminho real vai variar
```

## Mergulho Profundo

Historicamente, arquivos temporários são usados para prevenir a perda de dados durante falhas e para lidar com grandes conjuntos de dados que podem não caber na memória. Em Rust, a biblioteca `tempfile` é uma escolha popular para lidar com arquivos temporários, pois lida automaticamente com a exclusão de arquivos quando eles saem de escopo ou o programa termina. Alternativas incluem gerenciar manualmente arquivos temporários (menos seguro) ou usar outras bibliotecas com funcionalidades similares.

Detalhes de implementação são importantes: `tempfile()` cria um arquivo anônimo, enquanto `NamedTempFile::new()` gera um arquivo com um nome que pode ser recuperado ou inspecionado no sistema de arquivos. Os arquivos são, por padrão, criados no diretório especificado pela variável de ambiente `TMPDIR`, ou `/tmp` em sistemas Unix.

## Veja Também

- Documentação do módulo `std::fs` de Rust para operações de arquivo na [Standard Library](https://doc.rust-lang.org/std/fs/)
- Guia sobre como tratar erros de I/O em Rust na [Rust By Example](https://doc.rust-lang.org/rust-by-example/error/multiple_error_types/wrap_error.html)
