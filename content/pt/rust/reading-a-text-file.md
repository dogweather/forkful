---
title:    "Rust: Lendo um arquivo de texto"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Por que ler arquivos de texto usando Rust?

Existem várias razões pelas quais alguém pode querer ler um arquivo de texto usando Rust. Isso pode ser importante, por exemplo, ao trabalhar com conjuntos de dados ou ao criar aplicativos que dependem de entradas externas. Além disso, ler arquivos de texto também pode ser útil para realizar tarefas básicas de manipulação de dados, como filtrar e classificar informações.

## Como fazer isso?

Aqui estão algumas maneiras de ler arquivos de texto usando Rust.

```
use std::fs::File;
use std::io::{self, BufRead};

fn main() {
    // Abrindo o arquivo de texto
    let file = File::open("arquivo.txt").unwrap();
    let reader = io::BufReader::new(file);

    // Lendo cada linha e imprimindo na tela
    for line in reader.lines() {
        println!("{}", line.unwrap());
    }
}
```

Este é um exemplo simples de como ler um arquivo de texto linha por linha usando Rust. O resultado será a impressão de cada linha do arquivo no console.

## Profundidade na leitura de arquivos de texto

Além do método mencionado acima, existem outras maneiras de ler arquivos de texto usando Rust. Por exemplo, você também pode usar a biblioteca `std::fs` para ler todo o conteúdo do arquivo de uma só vez, ou usar a biblioteca `csv` para trabalhar especificamente com arquivos CSV.

Outro aspecto importante na leitura de arquivos de texto é considerar o uso de tratamento de erros adequado, para lidar com possíveis problemas como arquivos ausentes ou permissões insuficientes. Felizmente, Rust possui recursos embutidos para gerenciamento de erros, tornando essa tarefa mais fácil para os programadores.

## Veja também

- [Documentação oficial do Rust sobre I/O](https://doc.rust-lang.org/book/ch12-00-an-io-project.html)
- [Guia de referência da biblioteca `std::fs`](https://doc.rust-lang.org/std/fs/)
- [Pacote `csv` para trabalhar com arquivos CSV em Rust](https://github.com/BurntSushi/rust-csv)

Esperamos que este artigo tenha sido útil para você entender melhor como ler arquivos de texto usando Rust. Continue explorando e aprendendo sobre a linguagem para aprimorar suas habilidades de programação!