---
title:    "Rust: Verificando se um diretório existe"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe em Rust?

A verificação se um diretório existe pode ser uma tarefa importante em alguns programas escritos em Rust. Isso pode ser útil para garantir que os arquivos necessários estejam disponíveis antes de executar uma determinada função ou para evitar erros desnecessários. Portanto, aprender a verificar a existência de um diretório pode ajudar a melhorar a eficiência e a confiabilidade de seu código.

## Como fazer a verificação em Rust

A verificação de diretório é feita através do uso da biblioteca padrão "std::fs", que possui uma função chamada "metadata()". Esta função nos permite obter informações sobre um determinado arquivo ou diretório, incluindo se ele existe ou não. Vamos ver um exemplo de como usar essa função:

```Rust
use std::fs;

fn main() {
    let path = "documentos/relatorio.txt";
    let metadata = fs::metadata(path);

    match metadata {
        Ok(m) => {
            // verifica se é um diretório
            if m.is_dir() {
                println!("O diretório existe!");
            } else {
                println!("O diretório não existe!");
            }
        },
        Err(e) => println!("Erro ao verificar o diretório: {}", e),
    }
}
```

Se o arquivo "relatorio.txt" estiver presente na pasta "documentos", a saída será "O diretório existe!". Caso contrário, a saída será "O diretório não existe!".

## Aprofundando na verificação de diretório em Rust

Além de verificar apenas a existência de um diretório, a função "metadata()" também fornece outras informações úteis, como tamanho, data de criação e permissões do arquivo. Esses dados podem ser acessados usando os métodos disponíveis na estrutura de metadados retornada pela função.

Além disso, existe também a função "symlink_metadata()" que permite verificar a existência de um link simbólico em vez de um diretório em si. Isso pode ser útil em situações específicas onde um link simbólico é usado em vez de um diretório real.

## Veja também

- [Documentação da biblioteca padrão do Rust sobre a função "metadata()"](https://doc.rust-lang.org/std/fs/fn.metadata.html)
- [Tutorial de Rust para iniciantes](https://www.rust-lang.org/pt-BR/learn/get-started)
- [Exemplos práticos de uso de "metadata()" em programas Rust](https://crates.io/search?q=metadata)