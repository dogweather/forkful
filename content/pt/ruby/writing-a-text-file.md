---
title:                "Ruby: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

##Por que

Escrever um arquivo de texto pode parecer uma tarefa simples, mas é uma habilidade importante em qualquer linguagem de programação, incluindo Ruby. Ao aprender a escrever um arquivo de texto em Ruby, você será capaz de armazenar dados de forma organizada, criar e ler arquivos de configuração, entre outras funcionalidades úteis.

##Como Fazer

Para escrever um arquivo de texto em Ruby, utilizaremos o método `File.open()`, que possui três parâmetros: o nome do arquivo, o modo de operação e o bloco de código que será executado para escrever o arquivo.

```
File.open("arquivo.txt", "w") do |file|
  file.puts "Olá, mundo!"
end
```

Este código criará um arquivo de texto chamado "arquivo.txt" e irá escrever a string "Olá, mundo!" dentro dele. O parâmetro "w" indica que o arquivo será aberto no modo de escrita.

Para escrever mais de uma linha no arquivo, podemos utilizar o método `file.write()`. Por exemplo, para criar uma lista de frutas em nosso arquivo, podemos fazer o seguinte:

```
File.open("arquivo.txt", "w") do |file|
  file.write "Maçã\n"
  file.write "Banana\n"
  file.write "Morango\n"
end
```

Isso criará um arquivo de texto com as seguintes linhas:

```
Maçã
Banana
Morango
```

##Mergulho Profundo

Ao escrever um arquivo de texto em Ruby, também podemos utilizar o método `file.puts()` para adicionar uma nova linha após cada texto que escrevemos no arquivo. Por exemplo:

```
File.open("arquivo.txt", "w") do |file|
  file.puts "Primeira linha"
  file.puts "Segunda linha"
  file.puts "Terceira linha"
end
```

Isso criará um arquivo de texto com as seguintes linhas:

```
Primeira linha
Segunda linha
Terceira linha 
```

Além disso, podemos utilizar outros modos de operação do método `File.open()`, como "a" para adicionar texto no final do arquivo, e "r+" para leitura e escrita simultâneas.

##Veja Também

- [Ruby: How to Create and Write to a File](https://www.rubyguides.com/2015/05/working-with-files-ruby/)
- [Ruby File Class](https://ruby-doc.org/core-2.6.3/File.html)
- [Working with Files in Ruby](https://www.pluralsight.com/guides/working-files-ruby)