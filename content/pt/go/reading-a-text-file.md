---
title:                "Go: Lendo um arquivo de texto."
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler arquivos de texto em Go?

Ler arquivos de texto é uma tarefa comum em muitos programas Go, porque muitas vezes precisamos extrair dados de arquivos para processá-los ou apresentá-los aos usuários. Portanto, é importante entender como fazer isso de forma eficiente e efetiva em Go.

## Como Fazer

Em Go, podemos ler um arquivo de texto usando a função `os.Open()` para abrir o arquivo e `bufio.NewScanner()` para criar um scanner que pode ler os dados do arquivo linha por linha. Aqui está um exemplo de código que usa essas funções:

```
arquivo, err := os.Open("dados.txt")
if err != nil {
   log.Fatal(err)
}
scanner := bufio.NewScanner(arquivo)
for scanner.Scan() {
   linha := scanner.Text()
   // fazer algo com a linha
}
if err := scanner.Err(); err != nil {
   log.Fatal(err)
}
```

Neste exemplo, usamos o `for` loop e a função `scanner.Text()` para ler cada linha do arquivo e realizar alguma ação com ela. É importante sempre verificar o erro `scanner.Err()` depois do loop, para garantir que não ocorreram erros durante a leitura do arquivo.

## Deep Dive

Além de ler um arquivo linha por linha, também podemos usar o pacote `ioutil` para ler todo o conteúdo de um arquivo de uma só vez. Isso pode ser útil quando estamos lidando com arquivos de texto pequenos ou que não precisam ser processados linha por linha.

Se estivermos trabalhando com arquivos de texto que contêm dados estruturados, como no formato CSV, podemos usar o pacote `encoding/csv` para fazer a leitura e a interpolação desses dados em estruturas adequadas em Go.

## Veja Também

- [Documentação Go: Package os](https://pkg.go.dev/os)
- [Documentação Go: Package bufio](https://pkg.go.dev/bufio)
- [Documentação Go: Package ioutil](https://pkg.go.dev/ioutil)
- [Documentação Go: Package encoding/csv](https://pkg.go.dev/encoding/csv)