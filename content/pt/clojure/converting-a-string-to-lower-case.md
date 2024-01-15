---
title:                "Convertendo uma string para minúsculas"
html_title:           "Clojure: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que converter uma string em letras minúsculas?

Existem várias razões para converter uma string em letras minúsculas quando se está trabalhando com programação. Uma das principais razões é a padronização, já que muitas vezes é necessário comparar strings e essa comparação pode ser afetada pelas letras maiúsculas e minúsculas. Além disso, algumas funções específicas só funcionam com strings em letras minúsculas, então a conversão é necessária para que o código funcione corretamente.

## Como fazer a conversão em Clojure

Para converter uma string em letras minúsculas em Clojure, utilizamos a função "lower-case". Veja um exemplo abaixo:

```Clojure
(lower-case "Olá Mundo")
```

A saída desse código será "olá mundo", todas as letras da string em letras minúsculas. Essa função também pode ser utilizada em conjunto com outras funções, como por exemplo:

```Clojure

(def texto "Bem vindo ao Universo")
(println (lower-case (str "Olá " texto "!")))
```

Nesse caso, a saída seria "olá bem vindo ao universo!". Como podemos notar, as letras maiúsculas da variável "texto" também foram convertidas para minúsculas.

## Profundidade na conversão de strings

A função "lower-case" é uma função básica em Clojure, mas é importante entender que ela pode ter comportamentos diferentes dependendo do tipo de dado que está sendo utilizado. Por exemplo, ao utilizarmos a função em um número, o resultado será diferente do esperado:

```Clojure
(lower-case 123)
```

A saída desse código será "123", já que a função não é capaz de converter um número em uma string. Além disso, é importante lembrar que a função só irá converter as letras que estão no alfabeto inglês, então caracteres especiais ou de outros idiomas não serão convertidos.

## Veja também

- Documentação sobre a função "lower-case" em Clojure: https://clojuredocs.org/clojure.core/lower-case
- Artigo sobre como trabalhar com strings em Clojure: https://www.infoworld.com/article/3483420/get-started-with-clojure-string-processing-examples.html