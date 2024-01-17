---
title:                "Lendo um arquivo de texto."
html_title:           "Go: Lendo um arquivo de texto."
simple_title:         "Lendo um arquivo de texto."
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## O quê e Porquê?

Ler um arquivo de texto é uma tarefa comum para programadores. Isso envolve abrir e analisar um arquivo de texto para obter informações úteis. Programadores geralmente fazem isso para ler dados de entrada, como configurações ou informações armazenadas em um arquivo de texto.

## Como fazer:

Vamos dar uma olhada em como ler um arquivo de texto usando Go. Primeiro, precisamos abrir o arquivo usando a função `Open` do pacote `os`. Em seguida, podemos usar um `Scanner` do pacote `bufio` para ler o arquivo linha por linha. Finalmente, podemos imprimir as linhas lidas usando a função `Println` do pacote `fmt`.
```Go
// Abrir o arquivo
arquivo, err := os.Open("arquivo.txt")
if err != nil {
    log.Fatal(err)
}

// Criar um scanner para ler o arquivo
scanner := bufio.NewScanner(arquivo)

// Ler o arquivo linha por linha
for scanner.Scan() {
    fmt.Println(scanner.Text())
}

// Verificar e lidar com erros de leitura
if err := scanner.Err(); err != nil {
    log.Fatal(err)
}
```
Exemplo de saída:
```
Esta é a primeira linha do arquivo.
Essa é a segunda linha do arquivo.
E esta é a terceira linha.
```

## Mergulho profundo:

A leitura de arquivos de texto é uma tarefa que tem sido usada há muito tempo pelos programadores. Antes do Go, outras linguagens de programação também possuíam recursos para ler arquivos de texto, como a função `open()` da linguagem C. No Go, além do método mostrado acima, também é possível usar o pacote `ioutil` para ler o arquivo inteiro de uma só vez.

## Veja também:

- Documentação do pacote `os`: https://golang.org/pkg/os/
- Documentação do pacote `bufio`: https://golang.org/pkg/bufio/
- Documentação do pacote `fmt`: https://golang.org/pkg/fmt/
- Documentação do pacote `ioutil`: https://golang.org/pkg/io/ioutil/