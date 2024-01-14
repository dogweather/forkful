---
title:    "Gleam: Escrevendo um arquivo de texto"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto?

Escrever um arquivo de texto é uma tarefa essencial na programação, especialmente em linguagens como o Gleam. Ele permite que você armazene dados e informações de forma organizada e acessível, além de ser uma maneira de preservar seu trabalho e compartilhá-lo com outros programadores.

## Como escrever um arquivo de texto em Gleam

Para escrever um arquivo de texto em Gleam, você precisa seguir alguns passos simples. Primeiro, abra seu editor de código preferido e crie um novo arquivo com a extensão .gleam. Em seguida, use a sintaxe ```write_file()``` para especificar o nome do arquivo que você deseja criar e o conteúdo que deseja escrever. Por exemplo:

```
Gleam
import gleam/io
import gleam/string

message = "Oi, leitores!"

write_file("mensagem.txt", message)
```

Para executar esse código, basta salvá-lo e executá-lo no terminal com o comando ```gleam run seu_arquivo.gleam```. Isso criará um arquivo chamado "mensagem.txt" com o conteúdo "Oi, leitores!" dentro dele.

## Mais detalhes sobre escrever arquivos de texto em Gleam

Há muitas outras coisas que você pode fazer ao escrever um arquivo de texto em Gleam, como adicionar variáveis ​​e caracteres especiais, ou até mesmo ler e modificar arquivos já existentes. Você também pode criar diretórios de arquivos usando a função ```create_directory()``` e especificar o local onde deseja salvar o arquivo usando a função ```file_path()```.

Lembre-se de que, ao escrever arquivos de texto em Gleam, é importante lidar adequadamente com erros e exceções. Use a função ```try_file()``` para capturar e lidar com quaisquer problemas que possam surgir ao escrever ou ler o arquivo.

## Veja também

- Documentação oficial do Gleam sobre escrever arquivos: [https://gleam.run/documentation/std.fs.html#write_file][1]
- Tutorial em vídeo sobre como escrever arquivos em Gleam: [https://www.youtube.com/watch?v=5gqKcQc1Y1w][2]

[1]: https://gleam.run/documentation/std.fs.html#write_file
[2]: https://www.youtube.com/watch?v=5gqKcQc1Y1w