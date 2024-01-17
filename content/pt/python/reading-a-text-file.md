---
title:                "Lendo um arquivo de texto"
html_title:           "Python: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## O que e por que?
Ler um arquivo de texto é um processo importante na programação que envolve a leitura de dados de um arquivo e armazená-los em sua forma original. Isso é útil para acessar e manipular informações armazenadas em um arquivo de texto, como dados de configuração ou dados de entrada para um programa. Programadores geralmente usam essa técnica para facilitar a leitura e processamento de grandes quantidades de dados.

## Como fazer:
```
# Exemplo de código para ler um arquivo de texto
with open('texto.txt', 'r') as arquivo:
    linhas = arquivo.readlines()
    for linha in linhas:
        print(linha)
```
```
# Saída de exemplo
Primeira linha do texto
Segunda linha do texto
Terceira linha do texto
```

## Aprofundando:
Ler arquivos de texto é uma parte essencial da programação desde os primórdios da linguagem Python. Antes da versão 3.0, era necessário usar a função `open` para abrir um arquivo e depois usar o método `readlines` para ler o conteúdo do arquivo. No entanto, a partir da versão 3.0 do Python, foi introduzido o `with` statement, que gerencia automaticamente o fechamento do arquivo após a leitura. Além disso, existem outras maneiras de ler e manipular arquivos de texto, como por exemplo, a utilização da biblioteca `csv` para lidar com dados tabulares de forma mais eficiente.

## Veja também:
- [Documentação oficial do Python sobre a leitura de arquivos] (https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Tutorial sobre como ler e escrever arquivos de texto em Python] (https://www.digitalocean.com/community/tutorials/how-to-handle-plain-text-files-in-python-3)
- [Vídeo tutorial sobre leitura de arquivos em Python] (https://www.youtube.com/watch?v=Uh2ebFW8OYM)