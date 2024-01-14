---
title:    "Ruby: Escrevendo um arquivo de texto"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que

Escrever arquivos de texto pode ser uma tarefa comum ao programar em Ruby, seja para criar logs, salvar dados de usuários ou até mesmo gerar relatórios. Aprender como escrever um arquivo de texto pode tornar seu código mais eficiente e organizado.

## Como Fazer

Para escrever um arquivo de texto em Ruby, primeiro precisamos abrir o arquivo usando o método `File.open()`. Dentro deste método, precisamos especificar o nome do arquivo e o modo de escrita, utilizando "w" para escrita ou "a" para anexar. Vamos dar uma olhada em um exemplo:

```Ruby
arquivo = File.open("dados.txt", "w") # abre o arquivo no modo de escrita
arquivo.puts("Este é um exemplo de texto que será escrito no arquivo.") # escreve no arquivo
arquivo.close # fecha o arquivo
```

Neste exemplo, criamos um arquivo chamado "dados.txt" e escrevemos uma linha de texto dentro dele usando o método `puts`. Ao final, fechamos o arquivo com o método `close`. Agora, se abrirmos o arquivo "dados.txt", veremos que o texto foi escrito dentro dele.

## Deep Dive

Existem vários métodos que podem ser utilizados para escrever em um arquivo de texto em Ruby, como `print` e `write`. Além disso, podemos utilizar o método `File.new()` para criar um novo arquivo ao invés de utilizar o método `File.open()`. Também é possível criar um arquivo dentro de uma pasta específica, apenas adicionando o caminho para a pasta antes do nome do arquivo. Por exemplo: `"pasta/dados.txt"`. Além disso, é importante lembrar de sempre fechar o arquivo após escrever nele, utilizando o método `close` ou passando um bloco de código para o método `File.open()`.

## Veja Também

- [Documentação oficial do Ruby sobre escrita de arquivos](https://www.rubydoc.info/stdlib/core/File#write-instance_method)
- [Tutorial de Ruby: Aprendendo sobre arquivos](https://www.rubyguides.com/2015/05/working-with-files-ruby/)
- [Códigos de exemplo para escrever em arquivos de texto em Ruby](https://www.rubyguides.com/2015/05/working-with-files-ruby/#Writing_to_Files)