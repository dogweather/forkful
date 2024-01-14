---
title:    "Python: Lendo um arquivo de texto"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto em Python?

Ler e manipular arquivos de texto é uma habilidade importante para qualquer programador Python. Com essa habilidade, é possível processar e analisar grandes quantidades de dados armazenados em um arquivo de texto. Além disso, a leitura de arquivos de texto pode ser útil para a criação de programas que requerem interação com o usuário através de entradas e saídas de texto.

## Como fazer isso?

Começaremos importando a função `open` do módulo `io`. A função `open` nos permite abrir um arquivo de texto para leitura ou escrita. Ao usar a função `open`, é importante especificar o caminho e o nome do arquivo que deseja abrir. Se o arquivo não estiver no mesmo diretório em que o seu código Python está sendo executado, é necessário especificar o caminho completo para o arquivo.

```
import io

arquivo = io.open("arquivo_de_texto.txt", "r")
```

Neste exemplo, usamos o modo de leitura "r" para abrir o arquivo. Também é possível usar o modo de escrita "w" para criar um novo arquivo de texto ou substituir o conteúdo existente em um arquivo. Agora que o arquivo está aberto, podemos usar o método `read` para armazenar seu conteúdo em uma variável.

```
conteudo = arquivo.read()
```

Você também pode usar o loop `for` para ler o arquivo linha por linha e imprimir seu conteúdo, como no exemplo abaixo:

```
# Loop pelas linhas do arquivo e imprimindo seu conteúdo
for linha in arquivo:
    print(linha)
```

Se você quiser ler uma quantidade limitada de caracteres por linha, pode usar o método `readline` da seguinte forma:

```
# Lê os primeiros 10 caracteres da primeira linha do arquivo
linha = arquivo.readline(10)
print(linha)
```

Não se esqueça de fechar o arquivo após terminar de lê-lo, usando o método `close`:

```
arquivo.close()
```

## Mergulhando mais fundo

Além da função `open`, o módulo `io` também oferece outras funções e métodos úteis para trabalhar com arquivos de texto. Por exemplo, o método `write` pode ser usado para escrever conteúdo em um arquivo aberto em modo de escrita.

```
arquivo = io.open("arquivo_de_texto.txt", "w")
arquivo.write("Olá, mundo!")
arquivo.close()
```

Você também pode usar o método `seek` para definir o cursor em uma determinada posição no arquivo, o que pode ser útil para ler ou escrever em uma parte específica do conteúdo do arquivo. Além disso, o módulo `io` oferece suporte para manipulação de encodings e caracteres especiais, o que é importante para garantir a correta leitura e escrita em diferentes sistemas operacionais.

## Veja também

- [Documentação oficial do Python para a leitura de arquivos de texto](https://docs.python.org/pt-br/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Explicação sobre o módulo io em Python](https://www.geeksforgeeks.org/python-io-module/)
- [Tutorial em vídeo sobre como trabalhar com arquivos de texto em Python](https://www.youtube.com/watch?v=Uh2ebFW8OYM)