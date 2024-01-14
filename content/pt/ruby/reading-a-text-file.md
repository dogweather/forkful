---
title:    "Ruby: Lendo um arquivo de texto"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto?

Ler arquivos de texto é uma habilidade importante em programação Ruby, especialmente quando se trabalha com dados externos ou em projetos que envolvem manipulação de dados. Ao aprender a ler um arquivo de texto, você estará expandindo suas habilidades de programação e aumentando sua capacidade de criar soluções eficientes.

## Como ler um arquivo de texto em Ruby

Para ler um arquivo de texto em Ruby, você precisa primeiro abrir o arquivo com o método `File.open()`. Em seguida, use o método `File.read()` para ler o conteúdo do arquivo e armazená-lo em uma variável. Aqui está um exemplo de código Ruby para ler um arquivo de texto chamado "exemplo.txt":

```Ruby
file = File.open("exemplo.txt") # abre o arquivo
contents = File.read(file) # lê o conteúdo do arquivo e o armazena na variável "contents"
puts contents # imprime o conteúdo do arquivo
file.close # fecha o arquivo
```

## Mergulho mais profundo

Existem várias maneiras de ler um arquivo de texto em Ruby, incluindo o uso dos métodos `File.readlines()` ou `IO.foreach()`. Além disso, você também pode especificar o modo de leitura ao abrir o arquivo, como "r+" para leitura e gravação ou "a+" para acrescentar conteúdo ao final do arquivo. É importante lembrar de fechar o arquivo após a leitura para evitar problemas com a memória.

## Veja também

- [Documentação oficial do Ruby sobre leitura de arquivos](https://ruby-doc.org/core/IO.html)
- [Tutorial sobre leitura de arquivos em Ruby](https://www.rubyguides.com/2015/05/reading-files-ruby/)
- [Exemplos práticos de leitura de arquivos em Ruby](https://stackify.com/ruby-read-from-file/)