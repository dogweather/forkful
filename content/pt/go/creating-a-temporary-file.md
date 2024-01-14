---
title:    "Go: Criando um arquivo temporário"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em Go?

Criar um arquivo temporário é uma tarefa comum em muitos programas em Go. Isso pode ser necessário para armazenar dados temporários durante a execução do programa ou para criar arquivos de saída que serão excluídos após o uso. Além disso, é uma boa prática de programação para garantir que os recursos sejam liberados corretamente.

## Como criar um arquivo temporário em Go

Para criar um arquivo temporário em Go, podemos usar a função `ioutil.TempFile()` do pacote `io/ioutil`. A sintaxe básica é a seguinte:

```Go
tempFile, err := ioutil.TempFile(diretorio, prefixo)
if err != nil {
    // tratamento de erro
}
```
O primeiro parâmetro, `diretorio`, é o diretório onde o arquivo temporário será criado. Geralmente usamos `""`, que cria o arquivo no diretório atual. O segundo parâmetro, `prefixo`, é um prefixo opcional para o nome do arquivo temporário, por exemplo, "temp_file". Isso ajudará a identificar o arquivo caso haja mais de um sendo criado ao mesmo tempo.

Em seguida, podemos escrever dados no arquivo temporário usando `tempFile.Write()`, e depois fechá-lo usando `tempFile.Close()`.

```Go
// escrevendo dados no arquivo temporário
_, err = tempFile.Write([]byte("Este é um arquivo temporário em Go."))
if err != nil {
    // tratamento de erro
}

// fechando o arquivo temporário
err = tempFile.Close()
if err != nil {
    // tratamento de erro
}
```

Se nenhum diretório for especificado, o arquivo temporário será criado na pasta padrão do sistema, geralmente `/tmp` no Linux e `%TEMP%` no Windows.

## Aprofundando-se na criação de arquivos temporários em Go

A função `ioutil.TempFile()` retorna um arquivo que já foi aberto, portanto, devemos sempre fechá-lo após o uso. Além disso, podemos usar `ioutil.TempDir()` para criar um diretório temporário, que é útil quando precisamos criar vários arquivos temporários relacionados. Também é importante usar a função `tempFile.Name()` para obter o nome do arquivo temporário e, em seguida, excluí-lo usando `os.Remove()` quando já não precisamos mais dele.

Ao criar arquivos temporários, devemos ter em mente que eles são excluídos automaticamente após o programa ser encerrado. Se quisermos manter o arquivo temporário mesmo após o encerramento do programa, podemos usar `tempFile.Close()` para fechá-lo e, em seguida, usar `tempFile.Name()` para obter o nome do arquivo e renomeá-lo para algo mais significativo.

## Veja também

- Documentação oficial do pacote `io/ioutil`: https://golang.org/pkg/io/ioutil/
- Tutorial sobre como criar um arquivo temporário em Go: https://dev.to/aurelievache/creating-temporary-files-in-go-7k6
- Discussão sobre o motivo de criarmos arquivos temporários em Go: https://stackoverflow.com/questions/26852667/why-create-a-temporary-file-in-golang
- Mais exemplos de uso do pacote `io/ioutil`: https://www.socketloop.com/tutorials/golang-manipulate-file-io-using-io-and-io-util