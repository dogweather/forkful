---
title:    "Python: Lendo um arquivo de texto"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto no Python?

Se você é novo na programação em Python, pode estar se perguntando por que alguém iria querer ler um arquivo de texto. A resposta é simples: os arquivos de texto são uma forma comum de armazenar dados, e a leitura deles é essencial para muitas tarefas de programação.

## Como fazer isso em Python

Ler um arquivo de texto no Python é bastante simples. Primeiro, precisamos abrir o arquivo usando a função built-in "open()", que recebe dois parâmetros: o nome do arquivo e o "modo de acesso" (leitura, escrita, etc.). Por exemplo, para abrir um arquivo chamado "dados.txt" em modo de leitura, usamos o seguinte código:

```Python
arquivo = open("dados.txt", "r")
```

Em seguida, podemos ler o conteúdo do arquivo usando o método "read()". Este método retorna todo o conteúdo do arquivo como uma única string. Por exemplo, para ler o conteúdo do arquivo que acabamos de abrir, usamos o seguinte código:

```Python
conteudo = arquivo.read()
print(conteudo) # imprime o conteúdo do arquivo
```

Podemos também ler o arquivo linha por linha usando o método "readline()". Este método retorna uma única linha do arquivo, e cada chamada subsequente irá retornar a próxima linha. Por exemplo, para imprimir cada linha do arquivo em uma nova linha, usamos o seguinte código:

```Python
linha1 = arquivo.readline()
linha2 = arquivo.readline()
print(linha1)
print(linha2)
```

Finalmente, quando terminarmos de ler o arquivo, devemos fechá-lo usando o método "close()". Isso é importante para liberar os recursos usados pelo arquivo. Por exemplo, para fechar o arquivo que abrimos anteriormente, usamos o seguinte código:

```Python
arquivo.close()
```

## Aprofundando no assunto

Além dos métodos mencionados anteriormente, o Python oferece uma variedade de outras funções e métodos para trabalhar com arquivos de texto. Por exemplo, podemos usar os métodos "write()" e "writelines()" para escrever conteúdo em um arquivo, e a função "input()" para capturar a entrada do usuário e escrevê-la em um arquivo. Além disso, podemos usar a estrutura de controle "for" para percorrer todas as linhas de um arquivo e realizar operações em cada uma delas.

É importante lembrar que, quando lemos um arquivo de texto, ele é sempre lido como uma string. Isso significa que, se estivermos trabalhando com dados numéricos, precisaremos convertê-los para o tipo de dados apropriado antes de utilizá-los. O Python possui funções úteis, como "int()" e "float()", para realizar essas conversões.

## Veja também

Aqui estão alguns recursos adicionais para aprender mais sobre como ler arquivos de texto em Python:

- [Python Tutorial: Reading and Writing Files](https://www.programiz.com/python-programming/file-operation)
- [How to Read and Write Files in Python](https://realpython.com/read-write-files-python/)
- [Reading and Writing Files in Python](https://www.pythonforbeginners.com/files/reading-and-writing-files-in-python)