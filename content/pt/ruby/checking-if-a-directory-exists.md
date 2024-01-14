---
title:                "Ruby: Verificando se um diretório existe."
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que

Se você está aprendendo a programar em Ruby ou já tem experiência com a linguagem, provavelmente em algum momento precisou verificar se um diretório existe ou não. Esta é uma tarefa comum no desenvolvimento de software e pode ser útil em diversas situações. Neste artigo, vamos abordar o porquê de se verificar a existência de um diretório e como fazer isso em Ruby.

## Como fazer

Em Ruby, a maneira mais simples de verificar se um diretório existe é utilizando o método `Dir.exist?()` seguido do caminho do diretório que deseja verificar. Por exemplo, se você deseja checar se o diretório `documents` existe em seu sistema, pode utilizar o seguinte código:

```Ruby
Dir.exist?("documents")
```

Este método retorna `true` se o diretório existir e `false` caso contrário. Você também pode utilizar a forma abreviada `Dir.exists?()`.

## Deep Dive

Ao usar o método `Dir.exist?`, é importante ter em mente que ele retornará `true` mesmo se o caminho especificado não for um diretório, mas sim um arquivo. Isso ocorre pois ambos são considerados objetos de `Dir` em Ruby. Se você precisa ter certeza de que o caminho é um diretório e não um arquivo, pode utilizar o método `File.directory?()` em conjunto com `Dir.exist?()`, como mostrado no exemplo abaixo:

```Ruby
File.directory?("documents") && Dir.exist?("documents")
```

Outra forma de verificar a existência de um diretório é utilizando o método `File.exist?()`, que também pode ser usado para checar se um caminho leva a um arquivo ou diretório. No entanto, este método funciona apenas com caminhos absolutos, enquanto `Dir.exist?()` pode ser usado com caminhos relativos.

## Veja também

- [Documentação da classe Dir em Ruby](https://ruby-doc.org/core/Dir.html)
- [Documentação da classe File em Ruby](https://ruby-doc.org/core/File.html)
- [Tutorial sobre diretórios em Ruby](https://www.rubyguides.com/2018/07/ruby-dir/)