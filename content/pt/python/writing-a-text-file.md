---
title:                "Escrevendo um arquivo de texto"
date:                  2024-01-19
simple_title:         "Escrevendo um arquivo de texto"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Escrever um arquivo de texto em Python significa salvar dados em um arquivo no seu disco. Programadores fazem isso para persistir informação, automatizar tarefas ou como parte de um maior processo de input/output de um programa.

## Mãos à Obra:

```Python
# Abrindo e escrevendo em um arquivo
with open('exemplo.txt', 'w') as arquivo:
    arquivo.write('Olá, mundo!')

# Adicionando mais conteúdo
with open('exemplo.txt', 'a') as arquivo:
    arquivo.write('\nAcrescentando outra linha.')
```
**Saída**: Um arquivo chamado *exemplo.txt* é criado com o texto "Olá, mundo!" na primeira linha e "Acrescentando outra linha." na segunda.

## Mergulho Profundo:

Historicamente, armazenar dados em arquivos de texto é uma das formas mais básicas de persistência de dados. Alternativas incluem bancos de dados ou armazenamento em nuvem. A implementação detalhada envolve entender modos de abertura de arquivo como 'w' para escrita (que sobrescreve o arquivo) ou 'a' para append (que adiciona ao arquivo existente). Em Python, é essencial manejar arquivos com o contexto `with` para garantir que o arquivo seja fechado corretamente, mesmo se um erro ocorrer.

## Veja Também:

- [Documentação oficial do Python para I/O](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Tutorial Python de Real Python](https://realpython.com/read-write-files-python/)
- [Artigo sobre armazenamento de dados no Python](https://dbader.org/blog/python-file-io)
