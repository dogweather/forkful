---
title:    "Gleam: Verificando se um diretório existe"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Por que

Verificar se um diretório existe pode ser uma tarefa importante ao escrever um código em Gleam. Isso pode garantir que um determinado diretório esteja presente antes de realizar operações de leitura ou escrita de arquivos, evitando assim erros e problemas no seu programa.

## Como fazer

Para verificar se um diretório existe em Gleam, podemos utilizar a função `exists_dir` do módulo `Filesystem`. Essa função recebe como parâmetro o caminho do diretório que queremos verificar e retorna um valor booleano indicando se o diretório existe ou não.

```Gleam
import Filesystem

let directory_path = "meu_diretorio/"

let directory_exists = Filesystem.exists_dir(directory_path)

if directory_exists {
    io.println("O diretório existe!")
} else {
    io.println("O diretório não existe.")
}
```

No exemplo acima, o código primeiro importa o módulo `Filesystem` para ter acesso à função `exists_dir`. Em seguida, é definido o caminho do diretório a ser verificado e essa informação é passada como parâmetro para a função `exists_dir`. Por fim, é feita uma verificação condicional para imprimir a mensagem adequada dependendo do valor retornado pela função.

## Mergulhando mais fundo

Ao verificar se um diretório existe, o que está sendo feito é basicamente uma consulta ao sistema operacional para verificar se um determinado caminho corresponde a um diretório válido. Isso significa que, se o diretório não existir, é possível que ocorra um erro ao tentar acessá-lo em outras partes do código. Por isso, é importante realizar essa verificação antes de prosseguir com outras operações.

## Veja também

- [Documentação da função `exists_dir`no site oficial do Gleam](https://gleam.run/modules/filesystem#exists_dir)
- [Uma introdução ao Gleam para programadores em português](https://medium.com/@gleamlang/uma-introdu%C3%A7%C3%A3o-ao-gleam-para-programadores-em-portugu%C3%AAs-753f6db790ed)