---
title:                "Elixir: Escrevendo um arquivo de texto"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto em Elixir?

Escrever um arquivo de texto pode ser uma tarefa cotidiana, mas em Elixir, isso pode ser uma habilidade muito poderosa a ser dominada. Com a capacidade de criar, ler e manipular arquivos de texto, os programadores podem criar aplicativos altamente versáteis e eficientes. Além disso, o uso de arquivos de texto torna os dados facilmente acessíveis e manipuláveis, o que é essencial para muitas aplicações.

## Como fazer em Elixir

Para escrever um arquivo de texto em Elixir, primeiro precisamos abrir um novo arquivo ou abrir um já existente. Podemos fazer isso usando a função `File.open` e especificando o nome do arquivo, o modo de leitura/escrita e uma função de callback. Em seguida, podemos usar a função `IO.write` para escrever no arquivo e a função `File.close` para fechá-lo.

```
File.open("meu_arquivo.txt", [:write], fn file ->
  IO.write(file, "Meu primeiro arquivo de texto")
end)

File.close("meu_arquivo.txt")
```

Ao executar esse código, um novo arquivo de texto chamado `meu_arquivo.txt` será criado e a string "Meu primeiro arquivo de texto" será gravada nele. Podemos verificar o conteúdo do arquivo abrindo-o em um editor de texto.

## Profundidade: Escrevendo um arquivo de texto passo a passo

Agora, vamos nos aprofundar um pouco mais em como a escrita de arquivos de texto funciona em Elixir. Quando chamamos a função `File.open`, ela retorna um "arquivo" que é essencialmente uma estrutura de dados. Podemos pensar nessa estrutura como uma porta que nos permite acessar o arquivo subjacente. Usamos o operador "pipe" (`|>`) para passar essa estrutura de dados para a função `IO.write`.

Mas às vezes, queremos ter mais controle sobre o processo de escrita do arquivo. Por exemplo, podemos precisar gravar dados em diferentes partes do arquivo ou adicionar novas linhas de forma dinâmica. Nesses casos, podemos usar a função `IO.binwrite`, que nos permite especificar o offset (deslocamento) onde queremos gravar os dados dentro do arquivo.

```
File.open("meu_arquivo.txt", [:write], fn file ->
  # Primeiro vamos escrever "Elixir é incrível" no início do arquivo
  IO.binwrite(file, "Elixir é incrível", offset: 0)
  # Agora vamos adicionar uma nova linha no final do arquivo
  IO.binwrite(file, "\nMergulhando no Elixir", offset: :eof)
end)

File.close("meu_arquivo.txt")
```

Como mencionado anteriormente, também podemos usar a função `File.close` para encerrar a conexão com o arquivo. É importante fazer isso para garantir que não haja nenhuma gravação pendente e que o arquivo seja salvo corretamente.

## Veja também

- [Documentação oficial do Elixir sobre manipulação de arquivos] (https://hexdocs.pm/elixir/File.html)
- [Tutorial completo sobre manipulação de arquivos em Elixir] (https://www.tutorialspoint.com/elixir/elixir_files.htm)
- [Vídeo tutorial sobre escrita de arquivos em Elixir] (https://www.youtube.com/watch?v=Zaa-yoTJp_Q)