---
title:                "Lendo um arquivo de texto"
html_title:           "Bash: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Ler um arquivo de texto é uma estratégia básica para coletar e manipular dados em Python. Os programadores fazem isso para acessar dados armazenados em arquivos e usar esse conteúdo em seus programas.

## Como fazer:
Aqui está um exemplo simples de como ler um arquivo de texto em Python. Suponha que temos um arquivo chamado 'texto.txt' no mesmo diretório da nossa aplicação Python.

```Python
# abrindo o arquivo
arquivo = open('texto.txt', 'r')

# lendo o arquivo
print(arquivo.read())
```

Isso imprimirá o conteúdo do arquivo 'texto.txt' na tela. Se o arquivo contém "Olá, mundo!" então a saída será:

```Python
Olá, mundo!
```

## Mergulhando Mais a Fundo 
A função `open()` em Python existe há muito tempo - desde as primeiras versões da linguagem. No entanto, é importante ressaltar que essa função simples se tornou mais poderosa e flexível ao longo dos anos.

As alternativas ao método `open()` incluem funções como `os.open()` para um controle ainda mais preciso sobre como o arquivo é aberto.

Além disso, quando lemos um arquivo em Python usando `open()`, o Python cria um objeto de arquivo que fornece métodos e atributos necessários para ler, salvar e manipular o arquivo. É recomendável fechar sempre os arquivos depois de usá-los para liberar recursos do sistema.

```Python
arquivo = open('texto.txt', 'r')
print(arquivo.read())
arquivo.close()
```

## Veja Também
- Documentação oficial do Python sobre leitura e escrita de arquivos: https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files
- Compêndio de dicas da Real Python sobre leitura e escrita de arquivos: https://realpython.com/read-write-files-python/