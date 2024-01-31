---
title:                "Verificando se um diretório existe"
date:                  2024-01-20T14:56:44.758073-07:00
html_title:           "Fish Shell: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Verificar se um diretório existe é basicamente conferir se uma certa pasta está presente no sistema de arquivos. Programadores fazem isso para evitar erros ao tentar acessar, ler ou escrever em um diretório que não está lá.

## Como Fazer:
```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    dir := "./exemplo"

    if _, err := os.Stat(dir); os.IsNotExist(err) {
        fmt.Printf("O diretório %s não existe.\n", dir)
    } else {
        fmt.Printf("O diretório %s existe.\n", dir)
    }
}
```

Quando você executa este código, ele vai verificar se o diretório `exemplo` existe. Dependendo do resultado, ele vai imprimir:

```
O diretório ./exemplo não existe.
```

ou

```
O diretório ./exemplo existe.
```

## Mergulho Profundo
Historicamente, verificar a existência de um diretório é uma operação comum em vários sistemas operacionais, e as APIs para fazer isso mudaram pouco ao longo dos anos. Em Go, usamos a função `Stat` do pacote `os`, que retorna um erro se o diretório não existir. Essa é uma forma eficiente, já que não tentamos abrir o diretório.

Alternativas incluem usar a função `os.IsExist(err)`, que verifica se o erro ocorreu por um arquivo ou diretório realmente existir. No entanto, isso é raramente necessário para diretórios. Outra opção seria usar o pacote `path/filepath` para manipular caminhos de arquivos de uma maneira mais agnóstica ao sistema operacional.

Detalhes de implementação são simples: `os.Stat` não só verifica a existência, mas também retorna a informação sobre o arquivo ou diretório. Portanto, se você precisar de mais detalhes além da existência, você já terá eles disponíveis.

## Veja Também
- Documentação oficial da Go sobre o pacote `os`: https://golang.org/pkg/os/
- Mais sobre o tratamento de erros em Go: https://blog.golang.org/error-handling-and-go
- Tutorial Go sobre manipulação de arquivos e diretórios: https://golangbot.com/read-files/
