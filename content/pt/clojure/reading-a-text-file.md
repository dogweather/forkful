---
title:                "Lendo um arquivo de texto"
html_title:           "Clojure: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que
Ler arquivos de texto é uma tarefa comum em programação e pode ser útil em várias situações, como processamento de dados, criação de relatórios e leitura de configurações. Com o Clojure, essa tarefa pode ser realizada de forma simples e eficiente.

## Como Fazer
A leitura de arquivos de texto no Clojure é feita usando a função `slurp`. Essa função recebe como parâmetro o caminho para o arquivo e retorna o conteúdo do arquivo em forma de uma string. Por exemplo, para ler o conteúdo de um arquivo chamado "arquivo.txt" e exibir no console, podemos usar o seguinte código:

```Clojure
(def texto (slurp "arquivo.txt"))
(println texto)
```

Esse código irá imprimir todo o conteúdo do arquivo na tela. No entanto, se o arquivo for grande, pode ser mais útil ler o arquivo linha por linha. Para isso, podemos usar a função `line-seq`, que retorna uma sequência com as linhas do arquivo. Veja o exemplo abaixo:

```Clojure
(def linhas (line-seq (clojure.java.io/reader "arquivo.txt")))
(doseq [linha linhas]
  (println linha))
```

Note que nesse exemplo usamos a função `clojure.java.io/reader` para abrir o arquivo e obter um objeto leitor, que é passado como parâmetro para a função `line-seq`. Depois, usamos a função `doseq` para percorrer a sequência de linhas e imprimir uma a uma no console.

## Deep Dive
É importante ter em mente algumas considerações ao ler arquivos de texto no Clojure. Em primeiro lugar, a função `slurp` carrega todo o conteúdo do arquivo na memória, o que pode ser um problema com arquivos grandes. Nesse caso, é mais recomendável usar a função `line-seq` para ler linha por linha, evitando assim o carregamento completo do arquivo na memória.

Além disso, é importante prestar atenção ao formato do arquivo de texto. Se o arquivo estiver codificado em algum formato diferente do padrão UTF-8, podemos usar a função `with-open` para especificar o formato ao abrir o arquivo. Por exemplo:

```Clojure
(with-open [reader (clojure.java.io/reader "arquivo.txt" :encoding "ASCII")]
  (doseq [linha (line-seq reader)]
    (println linha)))
```

Nesse caso, estamos abrindo o arquivo especificando o formato `ASCII` e depois lendo linha por linha usando a função `line-seq`.

## Veja Também
- Documentação oficial sobre a função `slurp`: https://clojuredocs.org/clojure.core/slurp
- Documentação oficial sobre a função `line-seq`: https://clojuredocs.org/clojure.core/line-seq
- Artigo sobre leitura de arquivos no Clojure: https://www.baeldung.com/clojure-read-file