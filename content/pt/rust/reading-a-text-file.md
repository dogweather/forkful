---
title:                "Rust: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que

Se você é um programador iniciante ou experiente, provavelmente já teve que lidar com leitura de arquivos de texto em algum momento. Saber ler e manipular arquivos é uma habilidade essencial para qualquer programa. Neste artigo, vamos falar especificamente sobre como fazer isso em Rust.

## Como Fazer

Antes de começarmos, é necessário instalar a linguagem de programação Rust em seu sistema. Você pode encontrar instruções detalhadas sobre como fazer isso [aqui] (https://www.rust-lang.org/tools/install).

Para ler um arquivo de texto em Rust, primeiro precisamos abrir o arquivo usando a função `File::open ()`. Esta função recebe o caminho para o arquivo como parâmetro e retorna o arquivo em um objeto do tipo `File`.

```
Rust
let arquivo = File::open("caminho/do/arquivo.txt").expect("Não foi possível abrir o arquivo");
```

Em seguida, precisamos criar um objeto do tipo `BufReader` para armazenar o conteúdo do arquivo e permitir a leitura linha a linha.

```
Rust
let leitor = BufReader::new(arquivo);
```

Agora, podemos usar um loop para ler cada linha do arquivo e fazer o que quisermos com ela. Por exemplo, podemos imprimi-las no console.

```
Rust
for linha_em_texto in leitor.lines() {
    let linha = linha_em_texto.expect("Não foi possível ler a linha");
    println!("{}", linha);
}
```

## Aprofundando

Além de ler e imprimir as linhas de um arquivo de texto, existem muitas outras funções e métodos úteis para trabalhar com arquivos em Rust, como `write_all()` para escrever conteúdo em um arquivo, `metadata()` para obter informações sobre o arquivo e `create()` para criar um novo arquivo.

Também é importante lembrar de fechar o arquivo após terminar de usá-lo, para evitar possíveis problemas de desempenho ou degradação do sistema. Para isso, basta usar o método `close()` no objeto `File` ou `BufReader`.

## Veja Também

- [Documentação oficial da linguagem Rust] (https://www.rust-lang.org/pt-BR/learn)
- [Exemplos de leitura de arquivos em Rust] (https://www.programadorspot.com.br/2016/04/26/arquivos-de-leitura-e-escrita-em-rust/)
- [Tutorial passo a passo sobre manipulação de arquivos em Rust] (https://dbrgn.ch/posts/2015/05/writing-a-file-in-rust/)