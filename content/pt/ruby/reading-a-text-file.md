---
title:    "Ruby: Lendo um arquivo de texto"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que

Ler um arquivo de texto é uma tarefa comum na programação Ruby e pode ser útil por diversos motivos, como ler dados armazenados em um arquivo ou fazer a leitura de um arquivo de configuração do seu programa.

## Como Fazer

Existem várias maneiras de ler um arquivo de texto em Ruby. Uma maneira é usando o método `File.open` que recebe o nome do arquivo e o modo de leitura como argumentos. Veja um exemplo:

```
```Ruby
file = File.open("arquivo.txt", "r")
puts file.read
```

Neste exemplo, o arquivo de texto "arquivo.txt" será aberto no modo de leitura ("r") e seu conteúdo será impresso no console usando o método `read`.

Outra opção é utilizar o método `File.readlines`, que lê o arquivo e retorna um array com cada linha do texto como um elemento. Veja:

```
```Ruby
lines = File.readlines("arquivo.txt")
puts lines[0] # imprime a primeira linha do arquivo
```

Você também pode iterar sobre o array de linhas para realizar alguma operação com cada uma delas.

## Mergulho Profundo

Ao ler um arquivo de texto em Ruby, é importante ter em mente que o processo de leitura pode ser influenciado pelo encoding do arquivo. Se o arquivo contém caracteres especiais, como acentos e cedilhas, é necessário utilizar o encoding correto para não ter problemas ao ler o arquivo.

Além disso, é importante lembrar de fechar o arquivo após a leitura, utilizando o método `close`, ou utilizando o bloco `do...end` com o método `File.open`, que irá fechar o arquivo automaticamente ao final da operação.

## Veja Também

- [Documentação do método File.open](https://ruby-doc.org/core/IO.html#method-c-open)
- [Documentação do método File.readlines](https://ruby-doc.org/core/IO.html#method-i-readlines)
- [Tutorial sobre leitura de arquivos em Ruby](https://www.rubyguides.com/2015/05/reading-a-file-in-ruby/)