---
title:                "Gleam: Criando um arquivo temporário"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário é útil em Gleam

Ao trabalhar com linguagens de programação, é comum encontrarmos situações em que precisamos criar arquivos temporários. Isso pode acontecer, por exemplo, quando estamos manipulando grandes quantidades de dados temporariamente ou quando queremos testar uma nova funcionalidade antes de implementá-la completamente. Em Gleam, criar um arquivo temporário é uma tarefa simples e pode trazer inúmeras vantagens para o desenvolvimento de seus programas.

## Como criar um arquivo temporário em Gleam

Para criar um arquivo temporário em Gleam, utilizamos a função `File.Temp` da biblioteca `gleam/io`. Vamos dar uma olhada em um exemplo de código:

```Gleam
import gleam/io

fn create_temp_file() {
  let file = File.Temp("my_temp_file.txt")
  File.write(file, "Este é um arquivo temporário!")
}
```

Neste exemplo, usamos a função `File.Temp` para criar um arquivo temporário com o nome "my_temp_file.txt". Em seguida, utilizamos a função `File.write` para escrever uma mensagem dentro do arquivo. Depois que nosso código é executado, podemos encontrar o arquivo temporário criado na pasta de trabalho atual com o conteúdo "Este é um arquivo temporário!". É importante lembrar que, ao finalizar a execução do programa, o arquivo temporário é automaticamente excluído.

## Mais informações sobre a criação de arquivos temporários em Gleam

Gleam possui uma poderosa funcionalidade de gerenciamento de arquivos, o que torna a criação de arquivos temporários muito mais fácil e segura. Além disso, é possível definir o local onde o arquivo temporário será criado e especificar o modo de abertura do arquivo.

Para saber mais sobre a criação de arquivos temporários em Gleam, consulte a documentação oficial: [https://gleam.run/libraries/io#temporary-files](https://gleam.run/libraries/io#temporary-files)

## Veja também

- [Documentação oficial sobre a biblioteca `io` em Gleam](https://gleam.run/libraries/io)
- [Tutorial de introdução ao Gleam](https://gleam.run/getting-started)