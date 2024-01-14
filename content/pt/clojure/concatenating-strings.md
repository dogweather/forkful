---
title:                "Clojure: Concatenando strings"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que

Concatenar strings é uma prática comum na programação e pode ser útil em diversas situações, como na criação de mensagens personalizadas ou na manipulação de dados.

## Como Fazer

Para concatenar strings em Clojure, podemos utilizar a função `str`. Esta função recebe como argumento uma ou mais strings e retorna uma única string com a junção de todas elas.

Um exemplo simples de uso da função `str` seria o seguinte:

```Clojure
(str "Hello" " " "world") ; Saída: "Hello world"
```

Também é possível concatenar qualquer tipo de dado junto com as strings, basta convertê-los para string utilizando a função `str` antes de realizar a concatenação.

```Clojure
(def name "João")
(str "Hello " name "!") ; Saída: "Hello João!"
```

## Aprofundando

Na verdade, quando utilizamos a função `str` estamos utilizando o operador `clojure.string/str`. Internamente, esta função realiza um "join" entre as strings utilizando o separador padrão (espaço em branco). Porém, podemos especificar um separador diferente como segundo argumento da função.

Além disso, a função `str` também aceita coleções como argumentos, realizando a concatenação de todos os elementos da coleção.

Exemplos:

```Clojure
(str "Hello" " " "world") ; Saída: "Hello world"

(str "|" ["Hello" "world"]) ; Saída: "Hello|world"

(str ", " ["Hello" "world"]) ; Saída: "Hello, world"
```

## Veja também

- [Documentação oficial da função str em Clojure](https://clojuredocs.org/clojure.string/str)
- [Tutorial de Clojure na Wikibooks (em português)](https://pt.wikibooks.org/wiki/Clojure/Introdu%C3%A7%C3%A3o)
- [Curso Gratuito de Clojure na Udemy (em português)](https://www.udemy.com/course/curso-de-clojure/)