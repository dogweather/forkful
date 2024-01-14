---
title:    "Rust: Criando um arquivo temporário."
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em Rust?

Criar um arquivo temporário pode ser útil em muitas situações, principalmente quando estamos lidando com manipulação de arquivos ou quando queremos armazenar dados temporários durante a execução do programa. Em Rust, há uma maneira simples e elegante de criar e manipular arquivos temporários. Neste post, vamos explorar como fazer isso usando a biblioteca padrão do Rust.

## Como fazer em Rust

Para criar um arquivo temporário em Rust, vamos primeiro importar a biblioteca `std::fs` e a macro `tempfile::Builder`:

```Rust
use std::fs;
use tempfile::Builder;
```

Agora, usamos a função `Builder::new()` para criar um novo construtor de arquivos temporários e especificamos o prefixo e o sufixo do nome do arquivo que queremos gerar:

```Rust
let temp_file = Builder::new()
    .prefix("my_temp_file_")
    .suffix(".txt")
    .tempfile()
    .expect("Erro ao criar arquivo temporário");
```

Agora, temos um arquivo temporário chamado `my_temp_file_xxxxxx.txt` (onde os "x" representam caracteres aleatórios) criado no diretório atual do nosso programa. Podemos acessar e manipular esse arquivo usando os métodos da biblioteca `fs`. Por exemplo, podemos escrever e ler dados do arquivo:

```Rust
let data = "Este é um arquivo temporário gerado pelo Rust";
fs::write(&temp_file, data)
    .expect("Erro ao escrever no arquivo temporário");

let contents = fs::read_to_string(&temp_file)
    .expect("Erro ao ler o arquivo temporário");

println!("Conteúdo do arquivo: {}", contents);
```

A saída do programa seria:

```
Conteúdo do arquivo: Este é um arquivo temporário gerado pelo Rust
```

Por fim, podemos usar o método `persist` para converter o arquivo temporário em um arquivo permanente no sistema de arquivos:

```Rust
let persisted_file = temp_file.persist("my_persisted_file.txt")
    .expect("Erro ao persistir arquivo temporário");

println!("Arquivo permanente: {:?}", persisted_file);
```

Agora, teremos um novo arquivo permanente no diretório atual chamado `my_persisted_file.txt`. E quando o programa terminar de executar, o arquivo temporário será automaticamente excluído.

## Profundando mais

A biblioteca `tempfile` também nos oferece mais opções e métodos para gerenciar nossos arquivos temporários. Podemos especificar o diretório onde queremos que o arquivo seja criado, bem como outras configurações como permissões e tamanho máximo do arquivo.

Além disso, podemos usar a macro `tempdir::TempDir` para criar um diretório temporário em vez de um arquivo. Isso pode ser útil quando temos um conjunto de arquivos ou dados para armazenar temporariamente.

## Veja também

- Documentação oficial da biblioteca `tempfile`: https://docs.rs/tempfile/
- Documentação oficial da biblioteca `tempdir`: https://docs.rs/tempdir/
- Tutorial sobre manipulação de arquivos em Rust: https://www.youtube.com/watch?v=BAIqlF9eBtY&ab_channel=TraversyMedia