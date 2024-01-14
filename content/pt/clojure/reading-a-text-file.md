---
title:                "Clojure: Lendo um arquivo de texto"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto?

Ler um arquivo de texto é uma habilidade básica para qualquer programador de Clojure. Isso permite que você acesse informações armazenadas em um formato simples e universalmente suportado. Além disso, pode ser útil para processar grandes conjuntos de dados ou interagir com bibliotecas externas que armazenam informações em arquivos de texto.

## Como fazer

Ler um arquivo de texto em Clojure é uma tarefa simples e direta. Você pode usar a função `read-string` para ler um arquivo de texto e armazenar seu conteúdo em uma variável.

```
Clojure
(def conteudo (read-string "meu_arquivo.txt"))
```

Isso irá ler o arquivo `meu_arquivo.txt` e armazenar seu conteúdo na variável `conteudo`. Em seguida, você pode usar a função `println` para imprimir seu conteúdo no console.

```
Clojure
(println conteudo)
```

## Aprofundando-se

Existem várias maneiras de ler um arquivo de texto em Clojure. Além da função `read-string`, você também pode usar a função `slurp` para ler o conteúdo de um arquivo diretamente em uma string. Por exemplo:

```
Clojure
(def conteudo (slurp "meu_arquivo.txt"))
```

Você também pode especificar o caminho absoluto ou relativo do arquivo. Além disso, é possível passar um modo de leitura opcional como segundo argumento da função `read-string` ou `slurp`, como `:utf-8` para especificar a codificação do arquivo de texto.

## Veja também

- [Documentação oficial de Clojure sobre leitura de arquivos](https://clojuredocs.org/clojure.java.io/reader)
- [Exemplos de leitura de arquivos em Clojure](https://gist.github.com/yogthos/2780362)
- [Tutorial de Clojure sobre ler e escrever arquivos](https://www.tutorialspoint.com/clojure/clojure_file_io.htm)