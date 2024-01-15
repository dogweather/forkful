---
title:                "Extraindo substrings"
html_title:           "Clojure: Extraindo substrings"
simple_title:         "Extraindo substrings"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que
 Substrings são trechos de uma string, ou seja, palavras ou caracteres que fazem parte de uma string maior. Extrair substrings é útil para realizar operações específicas em certas partes de uma string, como por exemplo, buscar informações em um texto longo ou manipular dados em formato de string.

## Como Fazer
Para extrair substrings em Clojure, podemos usar a função `subs`, que recebe três argumentos: a string original, o índice inicial e o índice final. Vamos ver um exemplo:

```
Clojure (subs "Olá mundo!" 0 3)
```

Este código irá extrair a substring que começa no índice 0 e termina no índice 2 (pois o índice final é exclusivo), resultando em "Olá". Podemos usar a mesma lógica para extrair partes de uma string com base em nossas necessidades. Veja mais um exemplo:

```
Clojure (subs "Lorem ipsum dolor sit amet" 6 11)
```

Este código irá extrair a substring que começa no índice 6 e termina no índice 10, resultando em "ipsum". Perceba que os espaços em branco também são contados, então devemos levar isso em consideração ao definir os índices.

## Mergulho Profundo
Além da função `subs`, também podemos usar outras funções em Clojure para extrair substrings de maneiras mais complexas. Podemos usar a função `str/split` para separar uma string em vários pedaços, com base em um caractere específico. Por exemplo:

```
Clojure (str/split "Palavras separadas por espaços" #"\s")
```

Este código irá retornar uma lista com as palavras separadas por espaço como elementos, resultando em ["Palavras" "separadas" "por" "espaços"]. Também podemos usar a função `str/join` para unir uma lista de strings em uma única string, com base em um caractere específico. Veja um exemplo:

```
Clojure (str/join "-" ["Palavras" "separadas" "por" "espaços"])
```

Este código irá unir as palavras separadas por hífens, resultando em "Palavras-separadas-por-espaços".

## Veja Também
- [Documentação oficial do subs em Clojure](https://clojuredocs.org/clojure.core/subs)
- [Funções de manipulação de string em Clojure](https://clojuredocs.org/clojure.core/string)