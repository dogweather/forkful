---
title:                "Verificando se um diretório existe"
html_title:           "Gleam: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Verificar se um diretório existe é uma função comum em programação que retorna se um diretório específico já existe no sistema. Fazemos isso para evitar erros ao tentar criar um diretório que já existe ou acessar um que não existe.

## Como Fazer: 
Aqui está um exemplo de como você pode fazer isso na linguagem de programação Gleam:

```Gleam
import gleam/filesystem

let check = filesystem.dir_exists("caminho/para/o/diretório")
case check {
  True ->
    io.println("O diretório existe!")
  False ->
    io.println("O diretório não existe!")
}
```
Quando executado, a saída para um diretório existente será:
``` 
O diretório existe!
```
E se o diretório não existir, a saída será:
``` 
O diretório não existe!
```

## Mergulho Profundo
Historicamente, a verificação da existência de um diretório sempre fez parte das operações de sistema de arquivos. Isso é crucial principalmente no desenvolvimento de aplicativos que lidam com leitura e gravação de arquivos.

In Gleam, a biblioteca 'filesystem' incorpora funções como 'dir_exists' para manipular o sistema de arquivos. Esta função retorna um 'boolean', onde 'True' significa que o diretório existe e 'False', o contrário.

Uma alternativa para 'dir_exists' seria usar 'list_dir' que lista os diretórios e arquivos existentes, e então checar se o diretório desejado está nessa lista. No entanto, para grandes sistemas de arquivos, isso seria ineficiente. 

## Veja Também 
A documentação completa para a biblioteca 'filesystem' em Gleam pode ser encontrada em [Gleam filesystem documentation](https://gleam.run/documentation/libraries/gleam-filesystem). 

Além disso, a documentação oficial da linguagem Gleam pode ser acessada em [Gleam documentation](https://gleam.run/documentation/). Ele é uma ótima fonte para entender mais sobre outras funcionalidades do Gleam!