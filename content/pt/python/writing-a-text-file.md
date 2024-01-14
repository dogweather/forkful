---
title:    "Python: Escrevendo um arquivo de texto"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto?

Escrever um arquivo de texto é uma tarefa essencial para qualquer programador. Isso permite que você armazene informações importantes e configurações em um formato legível por humanos, que pode ser acessado facilmente sempre que necessário. Além disso, arquivos de texto podem ser facilmente compartilhados e utilizados por outros programas.

## Como escrever um arquivo de texto em Python

Para escrever um arquivo de texto em Python, siga estes passos simples:

1. Abra o arquivo utilizando a função `open()` e especifique o nome e o modo de escrita. Por exemplo: `arquivo = open("arquivo.txt", "w")`.

2. Use o método `write()` para inserir os dados no arquivo. Por exemplo: `arquivo.write("Este é um exemplo de texto que será salvo no arquivo.")`.

3. Finalize a escrita e feche o arquivo utilizando o método `close()`. Por exemplo: `arquivo.close()`.

Aqui está o código completo que escreve um arquivo de texto:

````Python
arquivo = open("arquivo.txt", "w")
arquivo.write("Este é um exemplo de texto que será salvo no arquivo.")
arquivo.close()
````

O arquivo `arquivo.txt` agora conterá o texto "Este é um exemplo de texto que será salvo no arquivo".

## Profundidade no processo de escrita de um arquivo de texto

Existem algumas coisas importantes a serem lembradas ao escrever um arquivo de texto em Python.

- Certifique-se de especificar o modo de escrita correto ao abrir o arquivo. Caso contrário, você pode acabar sobrescrevendo um arquivo existente ou recebendo um erro.

- Use o método `write()` para inserir os dados no arquivo. Lembre-se de que esse método apenas aceita strings como argumento. Se você precisar escrever outros tipos de dados em um arquivo de texto, é necessário convertê-los para string primeiro.

- É importante chamar o método `close()` após finalizar a escrita no arquivo. Isso garante que todo o conteúdo seja salvo corretamente e que o arquivo seja fechado adequadamente.

## Veja também

- [Documentação oficial do Python sobre a função `open()`](https://docs.python.org/3/library/functions.html#open)
- [Guia para iniciantes sobre escrita de arquivos em Python](https://www.codementor.io/@ilyaas97/6-python-tips-to-improve-your-coding-skills-in-2020-nj3lzvik8)
- [Exemplos de código práticos de escrita de arquivos em Python](https://github.com/python/cpython/blob/3.9/Lib/tokenize.py)