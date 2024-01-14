---
title:                "Python: Lendo um arquivo de texto."
simple_title:         "Lendo um arquivo de texto."
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto em Python?

Existem várias razões pelas quais alguém pode querer ler um arquivo de texto em Python. Pode ser para obter informações valiosas de um conjunto de dados, como ler registros de vendas de uma empresa ou para automatizar tarefas repetitivas, como importar dados para um programa.

## Como fazer isso em Python:

```Python
# Abrir arquivo
arquivo = open("arquivo.txt", "r") # 'r' para leitura, 'w' para escrita, 'a' para adicionar conteúdo

# Ler arquivo e imprimir conteúdo linha por linha
for linha in arquivo:
    print(linha)

# Fechar arquivo
arquivo.close()
```

Exemplo de saída:
```
Olá, Mundo!
Este é um arquivo de texto.
Estou sendo lido em Python.
```

## Mergulhando mais fundo:

Além de simplesmente ler um arquivo de texto, é possível acessar e manipular seu conteúdo de maneira mais avançada. Por exemplo, usando o método `read()` é possível armazenar todo o conteúdo do arquivo em uma variável, ou utilizando o método `seek()` é possível definir um ponto específico do arquivo para iniciar a leitura.

## Veja também:

- [Documentação oficial do Python sobre manipulação de arquivos](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Tutorial sobre leitura de arquivos em Python](https://www.w3schools.com/python/python_file_handling.asp)
- [Exemplos de código para leitura de diferentes tipos de arquivos em Python](https://www.tutorialexample.com/python-read-write-file-read-file-extension-convert-file-format/)

**Veja também**:
- [Documentação oficial do Python sobre manipulação de arquivos](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Tutorial sobre leitura de arquivos em Python](https://www.w3schools.com/python/python_file_handling.asp)
- [Exemplos de código para leitura de diferentes tipos de arquivos em Python](https://www.tutorialexample.com/python-read-write-file-read-file-extension-convert-file-format/)