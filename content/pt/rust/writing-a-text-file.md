---
title:    "Rust: Escrevendo um arquivo de texto"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto é importante em Rust?

Rust é uma linguagem de programação cada vez mais popular, conhecida por sua segurança, velocidade e eficiência. Quando se trata de manipulação de arquivos, Rust oferece uma ótima opção com sua biblioteca padrão. Neste post, vamos explorar como escrever um arquivo de texto em Rust e entender por que isso pode ser útil em seu projeto.

## Como fazer

Escrever um arquivo de texto em Rust é um processo simples. Primeiro, é necessário importar a biblioteca `std::fs`, que é responsável por manipular arquivos. Em seguida, abra o arquivo com a função `File::create()` e forneça o caminho desejado e o nome do arquivo. Em seguida, é preciso usar o método `write()` para escrever o conteúdo no arquivo. Por fim, salve as alterações com o método `flush()` e feche o arquivo usando o método `close()`. Um exemplo completo pode ser visto abaixo:

```Rust
use std::fs::File;

fn main() {
    let mut file = File::create("arquivo.txt").expect("Falha ao criar o arquivo!");
    file.write(b"Olá, leitor! Este é um exemplo de arquivo de texto criado com Rust.")
        .expect("Falha ao escrever no arquivo!");
    file.flush().expect("Falha ao salvar alterações!");
}
```

Ao executar esse código, um arquivo chamado `arquivo.txt` será criado no diretório do projeto, contendo o texto fornecido no método `write()`. 

## Mergulho profundo

A biblioteca `std::fs` também oferece outras funcionalidades para manipulação de arquivos, como ler, renomear e excluir arquivos. Além disso, é possível especificar o modo de abertura do arquivo, como somente leitura, escrita ou leitura e escrita. Também é importante mencionar que é possível trabalhar com arquivos binários usando a biblioteca `std::io` em conjunto com `std::fs`.

Além disso, existem bibliotecas externas disponíveis na comunidade Rust que podem ajudar a simplificar ainda mais o processo de manipulação de arquivos, como a biblioteca `stdext` e a `io_utils`. Ambas oferecem funções adicionais e melhorias em relação à biblioteca padrão.

## Veja também

- Documentação oficial do Rust sobre manipulação de arquivos: [https://doc.rust-lang.org/std/fs/index.html](https://doc.rust-lang.org/std/fs/index.html)

- Biblioteca externa `stdext`: [https://crates.io/crates/stdext](https://crates.io/crates/stdext)

- Biblioteca externa `io_utils`: [https://crates.io/crates/io_utils](https://crates.io/crates/io_utils)

Com esse conhecimento, você estará pronto para manipular arquivos em seus projetos Rust de forma eficiente e segura. Experimente e divirta-se programando em Rust!