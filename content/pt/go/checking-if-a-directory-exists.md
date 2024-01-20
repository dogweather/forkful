---
title:                "Verificando se um diretório existe"
html_title:           "Go: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Verificar se um diretório existe é o ato de conferir se uma pasta específica existe no sistema de arquivos. Fazemos isso para nos protegermos contra erros e garantir que nossos programas se comportem corretamente ao interagir com o sistema de arquivos.

## Como fazer:

Execute o código abaixo para verificar se um diretório existe em Go.

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	_, err := os.Stat("/caminho/para/o/diretorio")

	if os.IsNotExist(err) {
		fmt.Println("O diretório não existe.")
	}
	else {
		fmt.Println("O diretório existe.")
	}
}
```

No código acima, a função `os.Stat()` retorna um erro `nil` se o caminho existir e o valor de erro se não existir. Em seguida, `os.IsNotExist(err)` se torna verdadeiro se o erro disser que o caminho não existe.

## Aprofundamento

A funcionalidade de verificação da existência de diretórios é antiga na história do desenvolvimento de software, existindo em quase todas as linguagens de programação por causa de sua utilidade. No Go, uma alternativa a `os.Stat()` e `os.IsNotExist()` é a função `ioutil.ReadDir()`. Esta função tenta ler o conteúdo de um diretório e retorna um erro caso o diretório não exista. Quanto à implementação, ao usar `os.Stat()`, o Go chama a função `stat()` da API do sistema operacional subjacente, o que pode variar dependendo do sistema operacional.

## Veja também

Para mais detalhes, confira os seguintes links:

1. Documentação oficial Go sobre o pacote os: https://golang.org/pkg/os/
2. Tópico relevante no Fórum de desenolvedores Go: https://forum.golangbridge.org/t/check-if-a-directory-exists/3747
3. Stack Overflow: Como verificar se um arquivo ou diretório existe? - https://stackoverflow.com/questions/10510691/how-to-check-whether-a-file-or-directory-exists