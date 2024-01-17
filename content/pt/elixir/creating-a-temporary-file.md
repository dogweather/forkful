---
title:                "Criando um arquivo temporário"
html_title:           "Elixir: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O que é e por que fazemos isso?
Criar um arquivo temporário é simplesmente criar um arquivo que será usado apenas temporariamente durante a execução de um programa. Programadores o fazem para armazenar dados temporariamente e evitar conflitos com outros arquivos já existentes.

## Como fazer:
Você pode criar um arquivo temporário usando a biblioteca `File` do Elixir. Basta chamar a função `tempfile/0` para gerar um nome de arquivo único e, em seguida, usar a função `open!/2` para abrir o arquivo e escrever dados nele. Aqui está um exemplo de código:

```
file = File.tempfile() 

File.open!(file, [:write], fn(file) -> 
	File.write(file, "Hello World") 
end)
```

O resultado será um arquivo temporário com o nome e conteúdo especificados.

## Profundidade:
Criar arquivos temporários é uma prática comum em muitas linguagens de programação e é usado principalmente para armazenar dados que precisam ser processados e, em seguida, descartados. Existem também bibliotecas que facilitam a criação e gerenciamento de arquivos temporários, como o `Tempfile` em Ruby.

Internamente, criar um arquivo temporário envolve a geração de um nome de arquivo único e a criação de um arquivo com esse nome no sistema de arquivos. Após o uso, o arquivo é excluído automaticamente. Portanto, não é necessário gerenciar manualmente a exclusão do arquivo.

## Veja também:
- Documentação oficial da biblioteca File do Elixir: https://hexdocs.pm/elixir/File.html
- Biblioteca Tempfile do Ruby: https://ruby-doc.org/stdlib-2.6.1/libdoc/tempfile/rdoc/Tempfile.html