---
title:                "Escrevendo um arquivo de texto"
html_title:           "Ruby: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

### O que & Porquê? 

Escrever um arquivo de texto é simplesmente armazenar informações em um arquivo específico no computador. Isso é útil para guardar dados, configurações ou qualquer tipo de informação que você precise acessar posteriormente. Programadores frequentemente escrevem arquivos de texto para armazenar configurações de seus programas, dados de teste ou até mesmo relatórios.

### Como fazer:

Escrever um arquivo de texto usando Ruby é bastante fácil. Primeiro, precisamos abrir o arquivo usando o método `File.open` e especificar o nome do arquivo e o modo de abertura como parâmetros. Em seguida, podemos escrever nosso texto usando o método `write` e fechar o arquivo com o método `close`. Veja um exemplo abaixo:

```Ruby
arquivo = File.open("meu_arquivo.txt", "w")
arquivo.write("Escrevendo um arquivo de texto usando Ruby!")
arquivo.close
```

Se você verificar o conteúdo do arquivo "meu_arquivo.txt", verá que a frase foi adicionada ao arquivo. Mas e se quisermos adicionar mais informações a um arquivo já existente? Podemos fazer isso usando o modo de abertura "a" (append) no `File.open`. Confira:

```Ruby
arquivo = File.open("meu_arquivo.txt", "a")
arquivo.write("\nAqui está um exemplo de adição de mais informações a um arquivo de texto existente.")
arquivo.close
```

O `\n` adiciona uma quebra de linha antes de escrever a nova informação. Você pode adicionar quantas linhas quiser dessa maneira.

### Mergulho Profundo:

Escrever arquivos de texto é uma tarefa bastante comum para programadores, pois é uma maneira fácil de armazenar e acessar informações. Além disso, também é possível ler arquivos de texto em Ruby usando os métodos `File.read` e `File.readlines`. Esses métodos permitem acessar e manipular a informação do arquivo de texto.

É importante lembrar que Ruby oferece outras opções para armazenar dados, como bancos de dados ou arquivos de configuração específicos, mas escrever arquivos de texto ainda é uma solução bastante versátil e simples.

### Ver também:

* [Documentação oficial do Ruby sobre arquivos](https://ruby-doc.org/core-3.0.0/File.html)
* [Exemplos de escrita e leitura de arquivos em Ruby](https://www.rubyguides.com/2015/05/working-with-files-ruby/)