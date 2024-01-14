---
title:    "Rust: Lendo argumentos da linha de comando"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando em Rust?

Se você é um programador iniciante ou até mesmo experiente em Rust, provavelmente já encontrou muitas situações em que precisou ler argumentos da linha de comando. Esses argumentos servem como entradas para o seu programa, permitindo que os usuários forneçam informações específicas ao executá-lo. Neste artigo, vamos explorar como ler argumentos da linha de comando em Rust e como isso pode ser útil no seu dia a dia de programação.

## Como fazer:

A leitura de argumentos da linha de comando em Rust é muito simples e pode ser feita utilizando a biblioteca padrão `std::env`. Para isso, é necessário importar essa biblioteca no início do seu código utilizando o comando `use std::env;`.

Agora, vamos ver como podemos ler os argumentos utilizando o método `args()`, que retorna um iterador contendo todos os argumentos passados na linha de comando. Veja o código abaixo:

```Rust
use std::env;

fn main() {
    // Criando um vetor com os argumentos
    let args: Vec<String> = env::args().collect();

    // Imprimindo o primeiro argumento (o nome do programa)
    println!("Nome do programa: {}", args[0]);

    // Imprimindo os argumentos fornecidos pelos usuários
    println!("Argumentos fornecidos:");
    for arg in args.iter().skip(1) {
        println!("{}", arg);
    }
}
```

A saída do código acima, caso seja executado com os argumentos `hello world`, será a seguinte:

```
Nome do programa: cli_arguments
Argumentos fornecidos:
hello
world
```

Percebemos que os argumentos fornecidos foram armazenados no vetor `args` e podemos acessá-los utilizando índices. Além disso, utilizamos o método `collect()` para converter o iterador em um vetor.

## Mergulho profundo:

Além do método `args()`, a biblioteca `std::env` também possui outras funções úteis para trabalhar com argumentos da linha de comando. Por exemplo, o método `args_os()` retorna um iterador contendo os argumentos como `OsString`, que é uma representação do sistema operacional. Isso pode ser particularmente útil em cenários onde as entradas do usuário contêm caracteres especiais.

Outra funcionalidade interessante é o método `current_dir()`, que retorna o diretório atual do programa. Isso pode ser útil se você precisar manipular arquivos dentro do diretório em que seu programa está sendo executado.

## Veja também:

- [Documentação oficial do Rust sobre a biblioteca `std::env`](https://doc.rust-lang.org/std/env/index.html)
- [Tutorial de Rust: Trabalhando com argumentos da linha de comando](https://www.elianiva.com.br/2019/11/08/rust-tutorial-trabalhando-com-argumentos-da-linha-de-comando/)