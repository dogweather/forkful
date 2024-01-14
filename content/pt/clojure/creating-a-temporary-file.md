---
title:                "Clojure: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que
A criação de arquivos temporários é uma prática comum na programação, especialmente em linguagens funcionais como Clojure. Esses arquivos temporários podem ser úteis para armazenar dados temporários, testar diferentes funcionalidades ou até mesmo como uma saída temporária em um processo.

## Como Fazer
Se você precisa criar um arquivo temporário em seu código Clojure, é possível fazê-lo facilmente usando a função `with-open`. Veja um exemplo abaixo:

```Clojure
(with-open [temp-file (java.io.File/createTempFile "clojure-blog" ".txt")]
    (.write temp-file "Este é um arquivo temporário gerado pelo Clojure!")
    (.getName temp-file))
```

Este código criará um arquivo temporário chamado "clojure-blog.txt" e escreverá a frase "Este é um arquivo temporário gerado pelo Clojure!" dentro dele. A função `with-open` garante que o arquivo será fechado corretamente após o uso.

## Deep Dive
Se você quiser personalizar ainda mais seu arquivo temporário, é possível passar parâmetros adicionais para a função `createTempFile`. Por exemplo, se você quiser definir um diretório específico para o arquivo ser criado, você pode fazer da seguinte maneira:

```Clojure
(with-open [temp-file (java.io.File/createTempFile "clojure-blog" ".txt" 
                            (io/file "/users/juliana/desktop"))]
    (.write temp-file "Este é um arquivo temporário gerado pelo Clojure!")
    (.getName temp-file))
```

Isso criará o arquivo temporário no diretório "desktop" do usuário "juliana". Além disso, você também pode definir permissões para o arquivo temporário, especificar o tipo de arquivo ou até mesmo adicionar um timestamp ao nome do arquivo.

## Veja Também
- Função `with-open`: https://clojuredocs.org/clojure.core/with-open
- Módulo `java.io`: https://clojuredocs.org/clojure.java.io
- Tutorial sobre arquivos temporários em Clojure: https://www.clojureforthewin.com/post/understanding-java-interop-with-clojure-io-modules-and-temporary-files/