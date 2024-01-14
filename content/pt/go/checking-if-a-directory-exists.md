---
title:                "Go: Verificando se um diretório existe"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe em Go

Ao escrever um programa em Go, pode ser necessário verificar se um diretório já existe ou não antes de prosseguir com a execução. Isso pode ser importante para garantir que o programa funcione corretamente e evite erros ao tentar acessar um diretório inexistente.

## Como fazer isso em Go

Em Go, podemos usar a função `os.Stat()` para verificar a existência de um diretório. O código abaixo mostra um exemplo de como fazer isso:

```
package main

import (
    "fmt"
    "os"
)

func main() {
    // Definir o caminho do diretório que será verificado
    path := "/caminho/do/diretório"

    // Usar a função os.Stat() para verificar a existência do diretório
    if _, err := os.Stat(path); err == nil {
        fmt.Println("O diretório existe!")
    } else {
        fmt.Println("O diretório não existe.")
    }
}
```

Se o diretório existir, a saída será "O diretório existe!". Caso contrário, a saída será "O diretório não existe.".

## Mergulho profundo

O `os.Stat()` é uma função útil para verificar a existência de um diretório, mas também pode ser usada para obter informações sobre o diretório, como o tamanho, permissões e data de modificação.

Além disso, é importante lembrar que a função `os.Stat()` também pode ser usada para verificar se um arquivo existe. Em caso afirmativo, a função retornará informações sobre o arquivo. Se não existir, um erro será retornado.

## Veja também

- Documentação oficial sobre a função `os.Stat()`: https://golang.org/pkg/os/#Stat
- Exemplo de verificação de existência de diretório no playground do Go: https://play.golang.org/p/a-fmUjWEKWj