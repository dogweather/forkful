---
title:                "Criando um arquivo temporário"
html_title:           "Go: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O que e por que criar um arquivo temporario?

Criar um arquivo temporario e simplesmente criar um arquivo temporario que pode ser usado temporariamente para armazenar dados ou informações. Os programadores geralmente usam esse recurso para armazenar dados temporarios que nao precisam ser permanentemente salvos.

## Como fazer:

```Go
// Importar a biblioteca "os"
import "os"

// Criar um arquivo temporario usando a funcao "TempFile"
arquivoTemp, err := os.TempFile("", "exemplo")

// Verificar se houve algum erro ao criar o arquivo
if err != nil {
    panic(err)
}

// Imprimir o caminho do arquivo temporario criado
fmt.Println("Caminho do arquivo temporario:", arquivoTemp.Name())

// Fechar o arquivo temporario criado
defer os.Remove(arquivoTemp.Name())
```

Exemplo de saída:
```
Caminho do arquivo temporario: /var/folders/2v/8cw1kpcx3tg57q0n0yvmy3m1gwx2y1/T/exemplo758371380
```

## Mais informacoes:

Criar arquivos temporarios e um recurso comum em linguagens de programacao. Ele permite que os programadores armazenem dados temporarios sem precisar se preocupar com onde ou como esses dados serao armazenados permanentemente. Uma alternativa ao uso de arquivos temporarios seria o uso de variaveis temporarias na memoria, mas esta solucao pode ser util em situacoes onde os dados sao muito grandes ou quando o programa precisa ser executado em diferentes maquinas.

Ao criar um arquivo temporario no Go, o prefixo "tmp" sera adicionado ao nome do arquivo para ajudar a identifica-lo. Alguns outros parametros podem ser adicionados a funcao ``TempFile``, como o caminho e o prefixo do arquivo, para personalizar ainda mais o arquivo temporario.

## Veja tambem:

- [Documentacao oficial - Pacote "os"](https://golang.org/pkg/os/#TempFile)
- [Tutorial - Como criar um arquivo temporario com Go](https://www.digitalocean.com/community/tutorials/how-to-create-temporary-files-in-go-pt)