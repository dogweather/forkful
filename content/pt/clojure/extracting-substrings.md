---
title:    "Clojure: Extraindo Substrings"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Por que extrair substrings é útil?

Extrair substrings é uma tarefa comum em programação que pode ser útil por diversos motivos. Por exemplo, pode-se utilizar essa técnica para:

- Manipulação de textos: ao extrair uma parte específica de um texto, podemos usá-la para realizar certas operações, como formatação, substituição de caracteres, etc.
- Verificação de dados: se precisamos garantir que um determinado padrão ou formato é seguido em uma string, podemos extrair uma substring e compará-la com uma expressão regular, por exemplo.
- Análise de dados: em casos onde temos dados em formato de strings, podemos extrair substrings específicas para realizar análises e obter informações relevantes.

## Como extrair substrings em Clojure

Em Clojure, podemos extrair substrings utilizando a função `subs`, que recebe três parâmetros: a string original, o índice de início e o índice de fim da substring que desejamos extrair.

Um exemplo simples de extração de substring seria o seguinte:

```
(def texto "Isso é um texto de exemplo")
(subs texto 5 10)

;; output: é um
```

Além de especificar índices numéricos, também podemos utilizar a função `str-index` para encontrar a posição de uma determinada palavra ou caracter na string e utilizá-la como parâmetro para a função `subs`.

## Aprofundando na extração de substrings

A função `subs` utiliza a indexação baseada em zero, ou seja, o primeiro caracter da string é considerado como índice 0. Além disso, se não especificarmos o índice de fim, a função irá extrair todos os caracteres a partir do índice de início até o final da string.

Também é possível utilizar valores negativos para os índices, o que irá contar a partir do final da string. Por exemplo:

```
(def texto "Isso é um texto de exemplo")
(subs texto 0 -2)

;; output: Isso é um texto de exemp
```

Outra função útil para trabalhar com substrings é a `substring`, que funciona de maneira semelhante à função `subs` mas aceita como parâmetros a string original, o índice de início e a quantidade de caracteres que desejamos extrair.

## Veja também

- Documentação oficial da função subs: https://clojuredocs.org/clojure.core/subs
- Documentação oficial da função substring: https://clojuredocs.org/clojure.core/substring
- Tutorial em vídeo sobre extração de substrings em Clojure: https://www.youtube.com/watch?v=aQfZdlspoKg&t=464s