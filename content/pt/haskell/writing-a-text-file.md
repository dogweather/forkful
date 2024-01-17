---
title:                "Escrevendo um arquivo de texto"
html_title:           "Haskell: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

# O Que e Por Que Escrever um Arquivo de Texto?

Escrever um arquivo de texto é basicamente o ato de criar e salvar um documento de texto em formato de arquivo. Programadores costumam escrever arquivos de texto para armazenar dados e informações que serão utilizados pelos seus programas.

# Como Fazer:

```
Haskell

-- Cria um arquivo de texto e escreve "Ola mundo!" dentro dele.
main = do
  writeFile "ola_mundo.txt" "Ola mundo!"
  putStrLn "Arquivo de texto criado com sucesso!" 
```

Output esperado: 
```
Ola mundo!
```

# Aprofundando:

Existem diversas formas de escrever arquivos de texto em linguagens de programação, mas em Haskell, a forma mais comum é utilizando a função `writeFile` que recebe como argumento o nome do arquivo e o conteúdo a ser escrito nele. É importante lembrar que essa função sobrescreve o conteúdo existente em um arquivo, caso ele já exista.

Uma alternativa para escrever um arquivo de texto em Haskell é utilizando a função `appendFile`, que adiciona o conteúdo ao final do arquivo, sem sobrescrevê-lo.

# Veja Também:

- [Funções de escrita de arquivos em Haskell](https://hackage.haskell.org/package/base/docs/System-IO.html#v:writeFile)
- [Tutorial sobre manuseio de arquivos em Haskell](https://haskell.fpcomplete.com/io)
- [Documentação oficial do Haskell](https://www.haskell.org/documentation/)