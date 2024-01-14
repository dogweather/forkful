---
title:                "Clojure: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Por que usar expressões regulares em Clojure?

As expressões regulares são uma ferramenta poderosa para a manipulação e validação de strings em qualquer linguagem de programação, incluindo Clojure. Se você trabalha com dados textuais complexos ou precisa extrair informações de grandes conjuntos de dados, o uso de expressões regulares pode economizar tempo e esforço.

# Como usar expressões regulares em Clojure

Para começar a usar expressões regulares em Clojure, basta importar o módulo "re" com o seguinte código:

```Clojure
(require '[clojure.string :as str])
```

Em seguida, você pode definir uma expressão regular usando a função "re-seq" e aplicá-la a uma string usando a função "re-find". Por exemplo, se quisermos encontrar e extrair uma data no formato dd/mm/aaaa de uma string, podemos usar o seguinte código:

```Clojure
(def data-regex #"\d{2}/\d{2}/\d{4}")
(def texto "Este é um texto com uma data: 25/12/2021.")
(re-find data-regex texto) ; retorna "25/12/2021"
```

Para substituir padrões de string por novos valores, podemos usar a função "re-replace", como mostrado neste exemplo:

```Clojure
(def telefone-regex #"\(\d{2}\) \d{4}-\d{4}")
(def texto "Meu número de telefone é (11) 9999-9999.")
(re-replace telefone-regex texto "+55 $1") ; retorna "Meu número de telefone é +55 (11) 9999-9999."
```

# Profundidade em expressões regulares em Clojure

Clojure oferece suporte para várias funcionalidades avançadas de expressões regulares, como correspondência de caracteres unicode, captura de grupos e substituições baseadas em funções. Você pode ler mais sobre estas funcionalidades no [guia oficial de expressões regulares de Clojure](https://clojure.org/guides/regex).

Além disso, se você quiser aprender mais sobre expressões regulares em geral, outros recursos úteis incluem [regex101](https://regex101.com/), que permite testar e depurar expressões regulares em tempo real, e [regular expressions cookbook](https://subscription.packtpub.com/book/application_development/9781783285478), um guia prático para a criação de expressões regulares em diversas linguagens de programação.

# Veja também

- [Guia oficial de expressões regulares de Clojure](https://clojure.org/guides/regex)
- [regex101](https://regex101.com/)
- [Regular Expressions cookbook](https://subscription.packtpub.com/book/application_development/9781783285478)