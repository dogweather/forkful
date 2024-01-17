---
title:                "Criando um arquivo temporário"
html_title:           "Gleam: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Por que & Por quê?
Criar um arquivo temporário é uma prática comum entre programadores, pois permite que dados sejam armazenados temporariamente em um local específico durante a execução de um programa. Isso é útil para manipular arquivos grandes, gerar logs e executar testes. 

# Como fazer:
Criar um arquivo temporário em Gleam é simples. Basta usar a função `File.tmp` e fornecer um caminho para onde o arquivo será criado. A seguir, temos um exemplo de código e o respectivo output:

```Gleam
temp_file = File.tmp("meu_arquivo.txt")
```
Output: `Created temporary file at: meu_arquivo.txt`

# Mergulho profundo:
A criação de arquivos temporários é uma técnica que vem sendo utilizada há anos na programação. Na era pré-computador, os programadores costumavam utilizar cartões perfurados ou papeis para armazenar temporariamente dados. Hoje em dia, existem diversas alternativas para criar arquivos temporários, como a utilização de memória RAM ou bancos de dados. Em Gleam, a função `File.tmp` é implementada de forma eficiente, pois utiliza a memória swap do sistema operacional para criar o arquivo temporário.

# Veja também:
- Documentação oficial da função `File.tmp`
- Tutorial sobre criação de arquivos temporários em Gleam