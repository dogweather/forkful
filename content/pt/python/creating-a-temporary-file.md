---
title:                "Python: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em Python

Criar arquivos temporários pode ser uma tarefa muito útil para os programadores Python. Esses arquivos são criados com o propósito de armazenar dados temporariamente enquanto um programa estiver sendo executado. Eles são especialmente úteis para armazenar dados que não precisam ser permanentemente salvos ou para evitar saturação de espaço em disco.

## Como criar um arquivo temporário em Python

A criação de um arquivo temporário em Python é muito simples. Primeiro, precisamos importar a biblioteca `tempfile`, que já vem incluída na instalação padrão do Python 3. Em seguida, utilizamos a função `NamedTemporaryFile()` para criar o arquivo temporário:

```Python
import tempfile

arquivo_temporario = tempfile.NamedTemporaryFile()
```

Com isso, um arquivo temporário será criado no diretório padrão do sistema operacional com um nome único. Caso seja necessário, também é possível especificar um diretório específico para a criação do arquivo.

Podemos escrever dados no arquivo temporário utilizando o método `write()`, assim como em qualquer outro arquivo em Python:

```Python
# Escrevendo no arquivo temporário
arquivo_temporario.write(b"Este é um texto de exemplo para o arquivo temporário")

# Lendo o conteúdo do arquivo temporário
conteudo = arquivo_temporario.read()
print(conteudo)

# Fechando o arquivo temporário
arquivo_temporario.close()
```

Ao fechar o arquivo temporário, ele será automaticamente apagado do sistema.

## Deep Dive: Mais informações sobre a criação de arquivos temporários

Ao criar um arquivo temporário, podemos especificar algumas opções para customizar o seu comportamento. A seguir, listamos algumas das opções mais úteis:

- `mode`: permite definir o modo de abertura do arquivo temporário, assim como na função `open()`;
- `suffix`: define uma extensão para o arquivo temporário;
- `prefix`: especifica um prefixo para o nome do arquivo temporário;
- `dir`: permite definir um diretório customizado para a criação do arquivo temporário.

Além disso, é importante sempre fechar o arquivo temporário após o seu uso, pois isso garante que ele será apagado do sistema automaticamente. Caso contrário, ele permanecerá ocupando espaço no disco.

## Veja também

- [Documentação oficial do módulo `tempfile` em Python](https://docs.python.org/3/library/tempfile.html)
- [Tutorial sobre arquivos temporários em Python](https://realpython.com/python-tempfile/)