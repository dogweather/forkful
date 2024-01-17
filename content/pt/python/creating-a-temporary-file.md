---
title:                "Criando um arquivo temporário"
html_title:           "Python: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O que e Por que?

Criar um arquivo temporário é uma prática comum entre os programadores para armazenar dados temporariamente. Isso permite que eles executem operações facilmente, sem precisar salvar os dados permanentemente ou gerar conflitos com arquivos existentes.

## Como fazer:

```Python
import tempfile

# Criar um arquivo temporário usando a biblioteca tempfile
temp_file = tempfile.TemporaryFile()

# Escrever dados no arquivo temporário
temp_file.write(b"Hello, world!")

# Ler e imprimir o conteúdo do arquivo temporário
temp_file.seek(0)
print(temp_file.read())

# Fechar o arquivo temporário
temp_file.close()
```

**Saída:**
```
b"Hello, world!"
```

## Deep Dive:

A criação de arquivos temporários é uma técnica antiga e amplamente utilizada em programação. Ela permite que o programador lide com dados temporários de forma eficiente sem prejudicar os arquivos existentes ou gerar sobrecarga de memória. Existem alternativas para a criação de arquivos temporários, como o uso de variáveis na memória, no entanto, isso pode causar problemas de gerenciamento de memória em casos de grandes quantidades de dados. A implementação da criação de arquivos temporários pode variar de acordo com a linguagem de programação utilizada.

## Veja também:

- [Documentação oficial do Python sobre tempfile](https://docs.python.org/3/library/tempfile.html)
- [Tutorial sobre a criação de arquivos temporários em Python](https://realpython.com/python-tempfile/)
- [Explicação sobre o uso de variáveis temporárias em programas](https://computer.howstuffworks.com/compare-variable-temporary.htm)