---
title:    "Clojure: Utilizando expressões regulares"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar expressões regulares em Clojure?

Expressões regulares são uma poderosa ferramenta de processamento de texto que permite buscar e manipular padrões de caracteres em uma string. Em Clojure, elas são especialmente úteis para tarefas que envolvem manipulação de dados, validação e limpeza de strings.

## Como usar expressões regulares em Clojure

Usar expressões regulares em Clojure é fácil e pode trazer muitos benefícios ao seu código. Primeiro, importe o namespace `clojure.string` para ter acesso às funções de manipulação de strings. Em seguida, você pode usar a função `re-pattern` para criar um objeto regex a partir de uma string de padrão.

Exemplo de código:

```Clojure
(require '[clojure.string :as str])
(def email-pattern (re-pattern #"^[a-z0-9._%+-]+@[a-z0-9.-]+\.[a-z]{2,}$"))
```

O código acima cria um objeto regex capaz de validar se uma string é um endereço de email válido. Agora, você pode usar esse objeto em conjunto com a função `re-match?` para verificar se uma determinada string corresponde ao padrão desejado. Veja um exemplo completo abaixo:

```Clojure
(if (str/re-match? email-pattern "example@email.com")
  (println "Email válido!")
  (println "Email inválido. Por favor, insira um email válido."))
```
Output: `Email válido!`

## Explorando mais a fundo as expressões regulares em Clojure

Clojure oferece várias funções úteis para trabalhar com expressões regulares, como `re-groups` para retornar as partes da string que correspondem a um determinado padrão, `re-matches` para encontrar todas as ocorrências de um padrão em uma string e `re-find` para encontrar a primeira ocorrência de um padrão.

Além disso, é possível usar flags como `re-matcher` para fazer buscas insensíveis a maiúsculas e minúsculas ou `re-seq` para obter uma sequência de todas as correspondências encontradas em uma string. Com um bom domínio das expressões regulares em Clojure, você pode criar códigos mais robustos e eficientes para manipular e validar strings.

## Veja também

- [Documentação oficial sobre expressões regulares em Clojure](https://clojure.github.io/clojure/clojure.string-api.html#clojure.string/re-matcher)
- [Regular Expressions in Clojure - Sitepoint](https://www.sitepoint.com/regular-expressions-clojure/)
- [Clojure: Expressões Regulares - Programação em Scala](https://dirlididi.com/blog/clojure/clojure-%E2%80%93-expressoes-regulares/)