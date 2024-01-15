---
title:                "Escrevendo um arquivo de texto"
html_title:           "Python: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que
Escrever um arquivo de texto é uma tarefa fundamental para qualquer programador Python. Ele permite armazenar informações de forma persistente e acessá-las facilmente a qualquer momento. Além disso, é uma ótima maneira de compartilhar dados com outros usuários ou aplicativos.

## Como fazer
Começar a escrever um arquivo de texto em Python é bem simples. Primeiro, importe o módulo `io` que permitirá a criação e manipulação de arquivos. Em seguida, use a função `open()` para criar um objeto de arquivo e especificar o caminho e o modo de acesso (leitura, gravação, etc.). Veja um exemplo básico abaixo:

```Python
import io

# criar um arquivo de texto chamado "exemplo.txt"
arquivo = open("exemplo.txt", "w")

# escrever uma linha no arquivo
arquivo.write("Olá, mundo!")

# fechar o arquivo
arquivo.close()
```

O código acima irá criar um arquivo chamado "exemplo.txt" e adicionará a frase "Olá, mundo!" dentro dele. Agora, podemos abri-lo com um programa de edição de texto e ver o resultado.

## Aprofundando
Existem alguns pontos importantes a serem lembrados quando se trata de escrever um arquivo de texto em Python. Primeiro, certifique-se de fechar o arquivo após terminar a escrita. Isso garante que todos os dados sejam armazenados corretamente e que o arquivo não fique bloqueado para outras operações. Além disso, é importante usar o modo `w` (escrita) ao criar o arquivo, caso contrário, o arquivo não será gravável.

Outra forma de escrever em um arquivo é usando o método `with` que gerencia automaticamente o fechamento do arquivo para nós. Veja um exemplo abaixo:

```Python
import io

# criar um arquivo de texto chamado "exemplo.txt" usando "with"
with open("exemplo.txt", "w") as arquivo:
    # escrever uma linha no arquivo
    arquivo.write("Olá, mundo!")
```

Por fim, vale ressaltar que arquivos de texto podem armazenar diferentes tipos de dados, desde strings até números e até mesmo objetos complexos. Isso torna os arquivos de texto uma ferramenta muito versátil e útil para armazenar e compartilhar dados em um projeto de programação.

## Veja também
- Documentação oficial do módulo `io`: https://docs.python.org/3/library/io.html
- Tutorial sobre manipulação de arquivos com Python: https://www.learnpython.org/en/File_IO
- Mais informações sobre o modo de acesso aos arquivos: https://www.w3schools.com/python/python_file_handling.asp