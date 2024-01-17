---
title:                "Verificando se um diretório existe."
html_title:           "Go: Verificando se um diretório existe."
simple_title:         "Verificando se um diretório existe."
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O que & Porquê?
Verificar se um diretório existe é o processo de determinar se um caminho especificado corresponde a um diretório real no sistema de arquivos. Os programadores realizam essa verificação para evitar erros durante a execução do programa ou para garantir que o programa funcione apenas com diretórios existentes.

## Como fazer:
Verificar se um diretório existe em Go é bastante simples, graças às funções nativas disponíveis. Veja um exemplo de código que utiliza a função `os.Stat` para verificar se um diretório existe:

```
func main() {
    _, err := os.Stat("/caminho/para/diretório")
    if err != nil {
        fmt.Println("O diretório não existe")
    } else {
        fmt.Println("O diretório existe")
    }
}
```

O output desse código será "O diretório existe" se o diretório especificado existir no sistema de arquivos. Caso contrário, o output será "O diretório não existe".

## Mergulho Profundo:
Verificar a existência de diretórios é uma prática comum entre os programadores, mas nem sempre foi tão fácil quanto é no Go. Em versões anteriores da linguagem, era necessário usar a função `syscall.Stat` para realizar essa verificação. O Go implementou a função `os.Stat` para facilitar o processo.

Caso você prefira utilizar uma biblioteca de terceiros, existem várias opções disponíveis, como a "filepath" ou a "fs". Ambas oferecem funções para verificar a existência de diretórios de forma mais fácil e conveniente.

Se você estiver executando o Go em um sistema operacional que não suporta o uso de diretórios (como o Windows anterior ao Vista), é possível simular a verificação criando um arquivo no diretório e utilizando a função `os.IsExist` para verificar se o erro retornado é devido ao diretório já existir.

## Veja também:
- Documentação oficial do Go para a função `os.Stat`: https://golang.org/pkg/os/#Stat
- Biblioteca de terceiros "filepath": https://golang.org/pkg/path/filepath/
- Biblioteca de terceiros "fs": https://github.com/golang/go/tree/master/src/io/fs