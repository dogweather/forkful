---
title:                "Criando um arquivo temporário"
html_title:           "Bash: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O Quê & Porquê?

Criar um arquivo temporário é o processo de gerar um arquivo que pode ser usado para armazenar informações temporariamente. Programadores fazem isso normalmente para testes, para manipular dados sem alterar o original, ou quando o programa precisa de um espaço para manusear uma grande quantidade de dados.

## Como Fazer:

Vamos usar a biblioteca `tempfile` para criar arquivos temporários em Rust. Primeiro, adicione a dependência no seu `Cargo.toml`.

```Rust
[dependencies]
tempfile = "3.0"
```
Agora, aqui está um exemplo simples de como criar um arquivo temporário:

```Rust
use tempfile::NamedTempFile;

fn main() {
    let temp_file = NamedTempFile::new().unwrap();

    println!("Arquivo temporário criado: {:?}", temp_file.path());
}
```

Quando você executa este código, ele criará um arquivo temporário e imprimirá o caminho do arquivo.

## Imersão Profunda:

A criação de arquivos temporários é uma prática comum usada desde os primeiros tempos da computação. No contexto histórico, alguns sistemas operacionais até oferecem seu próprio sistema de arquivos temporários.

Em termos de alternativas, você também pode usar a função `tempdir()` para criar um diretório temporário em vez de um arquivo.

Em relação aos detalhes de implementação, quando você cria um arquivo temporário em Rust com a biblioteca `tempfile`, o arquivo é automaticamente removido quando o `TempFile` é solto. Este comportamento é incrivelmente útil, já que lida automaticamente com a limpeza de arquivos temporários, impedindo o acúmulo de arquivos obsoletos.

## Veja Também:

Você pode verificar estes links para mais informações:

- Documentação Rust Tempfile: https://docs.rs/tempfile/3.0.7/tempfile/
- Guia do usuário Rust: https://doc.rust-lang.org/book/title-page.html
- Exemplos na Create a Temporary File in Rust: https://stackoverflow.com/questions/31192956/whats-the-de-facto-way-of-reading-and-writing-files-in-rust