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

### Por que ler um arquivo de texto em Python?

A leitura de arquivos de texto é uma tarefa muito comum em projetos de programação. Ao ler um arquivo de texto, é possível acessar e manipular os dados presentes nele. Isso é especialmente útil para armazenar e processar grandes quantidades de dados de forma organizada.

### Como ler um arquivo de texto em Python?

Para ler um arquivo de texto em Python, é preciso seguir alguns passos simples:

1. Primeiro, abra o arquivo de texto usando a função `open()`. Precisamos passar o caminho do arquivo como argumento, seguido pelo modo de operação - `r` para a leitura de um arquivo de texto.
```
arquivo = open("arquivo.txt", "r")
```

2. Em seguida, use o método `read()` para ler o conteúdo do arquivo e atribua-o a uma variável.
```
conteudo = arquivo.read()
```

3. Depois de terminar de usar o arquivo, é importante fechá-lo usando o método `close()`.
```
arquivo.close()
```

4. Por fim, podemos imprimir o conteúdo do arquivo para verificar se foi lido corretamente.
```
print(conteudo)
```

### Mergulho Profundo: Manipulando a leitura de um arquivo de texto

O método `read()` usado no exemplo anterior lê todo o conteúdo do arquivo de texto e o armazena em uma única string. No entanto, também é possível ler os dados linha por linha usando o método `readline()`. Além disso, se estivermos trabalhando com um arquivo muito grande, é mais eficiente ler os dados por partes usando o método `read(size)`.

Além disso, o modo de operação também pode ser alterado de `r` para `rb` ou `r+` para ler arquivos binários ou permitir a escrita no arquivo, respectivamente. Para mais informações sobre todas as possibilidades de leitura de um arquivo em Python, consulte a documentação oficial.

### Veja também

- [Documentação oficial do Python sobre a função `open()`](https://docs.python.org/3/library/functions.html#open)
- [Tutorial do RealPython sobre manipulação de arquivos em Python](https://realpython.com/read-write-files-python/)
- [Artigo do DigitalOcean sobre manipulação de arquivos em Python](https://www.digitalocean.com/community/tutorials/how-to-handle-plain-text-files-in-python-3)