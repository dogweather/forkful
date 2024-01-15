---
title:                "Escrevendo um arquivo de texto"
html_title:           "Go: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que

Escrever um arquivo de texto é uma atividade fundamental ao escrever programas. É a maneira de armazenar dados de forma persistente e acessá-los facilmente através do código.

## Como Fazer

Escrever um arquivo de texto é uma tarefa bastante simples em Go. Primeiro, precisamos criar um arquivo usando a função `Create` do pacote `os`. Passamos o nome do arquivo como parâmetro e ele será criado no diretório atual do programa.

```Go
file, err := os.Create("arquivo.txt")
if err != nil {
    log.Fatal(err)
}
```

Com o arquivo criado, precisamos escrever algum conteúdo nele. O pacote `bufio` fornece uma maneira conveniente de escrever dados em um arquivo.

```Go
writer := bufio.NewWriter(file)
texto := "Este é um texto de exemplo."
_, err = writer.WriteString(texto)
if err != nil {
    log.Fatal(err)
}
```

Não esqueça de salvar o conteúdo no arquivo e fechá-lo antes de finalizar o programa.

```Go
writer.Flush()
file.Close()
```

## Deep Dive

Ao escrever arquivos de texto, é importante entender os diferentes modos de escrita disponíveis. O exemplo anterior usou o modo "escrita única", que sobrescreve o conteúdo do arquivo a cada execução. Outra opção é o modo "append", que adiciona o conteúdo no final do arquivo sem remover o que já está lá.

```Go
file, err := os.OpenFile("arquivo.txt", os.O_APPEND|os.O_WRONLY, 0644)
```

Também é possível especificar permissões do arquivo ao cria-lo, utilizando o valor `0644`. Aqui, o primeiro "0" indica que estamos definindo as permissões básicas para todos os usuários, o "6" é para o dono com permissões de leitura e escrita, e o "4" é para todos os demais usuários com permissão apenas de leitura.

## Veja Também

- Documentação oficial do pacote `os`: https://golang.org/pkg/os/
- Documentação oficial do pacote `bufio`: https://golang.org/pkg/bufio/
- Guia de escrita de arquivos em Go: https://www.digitalocean.com/community/tutorials/how-to-read-and-write-files-in-go#choosing-the-scope-of-your-file-io-operations