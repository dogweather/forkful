---
title:                "Python: Escrevendo um arquivo de texto."
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto em Python

Escrever um arquivo de texto em Python é uma maneira simples e eficiente de armazenar informações que podem ser acessadas e usadas no futuro. É uma habilidade fundamental para qualquer programador, pois permite a criação e o gerenciamento de arquivos que podem ser lidos e modificados pelo código.

## Como fazer

Para criar um arquivo de texto em Python, você pode seguir os seguintes passos:

```
Python
# Abrir o arquivo no modo de escrita
arquivo = open("nome_do_arquivo.txt", "w")

# Escrever informações no arquivo
arquivo.write("Olá, mundo!")

# Fechar o arquivo
arquivo.close()

# Abrir o arquivo no modo de leitura
arquivo = open("nome_do_arquivo.txt", "r")

# Ler o conteúdo do arquivo
conteudo = arquivo.read()

# Imprimir o conteúdo na tela
print(conteudo)

# Fechar o arquivo
arquivo.close()

```

A saída deste código será:

```
Olá, mundo!
```

Você pode modificar o conteúdo do arquivo alterando o que está entre as aspas na função `write()`. Além disso, você também pode usar o modo de adicionar (`"a"`) para adicionar novas informações ao final do arquivo, ou o modo de leitura e escrita (`"r+"`) para ler e escrever no arquivo ao mesmo tempo.

## Mergulho profundo

Ao escrever um arquivo em Python, é importante ter em mente alguns conceitos:

- O modo de abertura do arquivo: como mencionado anteriormente, você pode usar diferentes modos para abrir o arquivo dependendo do que deseja fazer com ele.
- Codificação: é importante especificar a codificação do arquivo ao abrir ou criar um arquivo. Isso garantirá que os caracteres sejam exibidos corretamente ao ler ou escrever no arquivo.
- Manipulação de erros: ao lidar com arquivos, é sempre uma boa prática incluir uma lógica para tratar possíveis erros, como o arquivo não existir ou o caminho do arquivo ser inválido.

Para saber mais sobre a manipulação de arquivos em Python, você pode consultar a documentação oficial do Python ou alguns dos links abaixo.

## Veja também

- [Documentação oficial do Python sobre manipulação de arquivos](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Tutorial sobre manipulação de arquivos em Python](https://www.datacamp.com/community/tutorials/reading-writing-files-python)
- [Guia prático de manipulação de arquivos em Python (em inglês)](https://realpython.com/read-write-files-python/)