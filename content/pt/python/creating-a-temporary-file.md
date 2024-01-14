---
title:                "Python: Como criar um arquivo temporário"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário no Python?

Criar um arquivo temporário é uma tarefa comum em programação Python, especialmente quando estamos trabalhando com grandes quantidades de dados ou precisamos armazenar informações temporariamente. Uma das principais razões para criar um arquivo temporário é evitar sobrecarregar a memória do computador, já que esses arquivos são automaticamente excluídos quando não são mais necessários.

## Como criar um arquivo temporário no Python?

Existem várias maneiras de criar um arquivo temporário no Python. A seguir, mostraremos dois exemplos de código usando a biblioteca integrada "tempfile" que é comumente usada para essa tarefa.

Primeiro exemplo: criando um arquivo temporário

```python
import tempfile

# Cria um arquivo temporário com o prefixo "dados_" e o sufixo ".csv"
with tempfile.NamedTemporaryFile(prefix="dados_", suffix=".csv") as temp_file:
    # Escreve uma lista de números no arquivo temporário
    for i in range(1, 11):
        temp_file.write(str(i).encode())

    # Acessa o conteúdo do arquivo
    # É importante destacar que o arquivo é excluído automaticamente após ser fechado
    temp_file.seek(0)
    print(temp_file.read())
```

A saída desse código será a seguinte:

```python
b'12345678910'
```

Nesse exemplo, usamos o "NamedTemporaryFile" para criar um arquivo com o prefixo "dados_" e o sufixo ".csv". Precisamos converter o número em "bytes" antes de escrevê-lo no arquivo, já que o "write" aceita apenas dados em bytes.

Segundo exemplo: criando um diretório temporário

```python
import tempfile

# Cria um diretório temporário com o sufixo "temp"
with tempfile.TemporaryDirectory(suffix="temp") as temp_dir:
    # Cria um arquivo dentro do diretório temporário
    temp_file = tempfile.NamedTemporaryFile(dir=temp_dir)
    # Escreve uma mensagem no arquivo
    temp_file.write("Esse é um arquivo temporário criado dentro do diretório temporário!".encode())
    
    # Acessa o conteúdo do arquivo
    temp_file.seek(0)
    print(temp_file.read())
```

A saída desse código será a seguinte:

```python
b'Esse é um arquivo temporário criado dentro do diretório temporário!'
```

## Aprofundando no assunto

A biblioteca "tempfile" oferece uma infinidade de métodos além dos mostrados nos exemplos acima. Você pode definir o tamanho máximo do arquivo temporário, escolher o local onde o arquivo será criado e até mesmo controlar quando ele será excluído. Além disso, também é possível criar arquivos temporários usando a biblioteca "io" ou o comando "with open", dependendo da necessidade do seu projeto.

Criar e gerenciar arquivos temporários pode ser uma tarefa complexa, mas com a ajuda da biblioteca "tempfile" do Python, essa tarefa se torna muito mais simples e eficiente.

## Veja também

- [Documentação oficial da biblioteca tempfile](https://docs.python.org/3/library/tempfile.html)
- [Tutorial sobre como usar a biblioteca tempfile no Python](https://dev.to/davidakarsh/using-the-python-tempfile-module-5fbb)
- [Usando a biblioteca tempfile para criar gráficos temporários no Python](https://towardsdatascience.com/absolute-beginners-guide-to-etl-using-python-tempfile-7821fd474aa9)

Esperamos que este artigo tenha sido útil para você aprender como criar e gerenciar arquivos temporários no Python. Para mais informações e dicas sobre programação Python, não deixe de conferir nosso blog. Obrigado por ler!