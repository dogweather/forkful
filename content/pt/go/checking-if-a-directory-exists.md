---
title:    "Go: Verificando se um diretório existe"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe?

Verificar se um diretório existe é uma tarefa comum na programação Go e pode ser útil para garantir que seu código funcione corretamente. Isso também pode facilitar o gerenciamento de arquivos e diretórios no seu sistema.

## Como fazer isso:

Verificar se um diretório existe em Go é bastante simples. Você pode usar a função `os.Stat()`, que retorna um `os.FileInfo` com informações sobre o arquivo ou diretório, incluindo se ele existe ou não. Veja um exemplo de código abaixo:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    dir := "/Users/joao/diretorio"
    _, err := os.Stat(dir)
    // Se o diretório existe, err será nulo
    if err == nil {
        fmt.Println("O diretório existe!")
    } else {
        fmt.Println("O diretório não existe!")
    }
}
```

Se o diretório existir, a saída do programa será "O diretório existe!". Caso contrário, será "O diretório não existe!".

## Profundidade na verificação de diretórios

Existem outras funções em Go que você pode usar para verificar se um diretório existe, como `os.IsNotExist()`, `os.IsExist()` e `os.MkdirAll()`. Além disso, você também pode usar essas funções para criar um diretório se ele não existir.

Além disso, você pode usar a função `filepath` para lidar com caminhos de diretório e arquivo de forma mais eficiente.

Com as funções e métodos certos, verificar se um diretório existe pode facilitar o seu trabalho com arquivos e diretórios no Go.

## Veja também:

- [Tutorial: Trabalhando com arquivos e diretórios em Go](https://golang.org/pkg/os/)
- [Documentação oficial do pacote "os" em Go](https://golang.org/pkg/os/)
- [Guia de referência para manipulação de arquivo no Go](https://www.calhoun.io/working-with-files-in-go/)