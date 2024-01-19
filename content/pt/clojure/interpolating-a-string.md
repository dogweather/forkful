---
title:                "Interpolando uma string"
html_title:           "Java: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Interpolar uma string é um processo para substituir placeholders por valores em uma cadeia de texto. Programadores o usam para enriquecer mensagens e melhorar a legibilidade do código.

## Como Fazer:

Aqui está como você pode fazer a interpolação de strings em Clojure usando `format`:

```Clojure
(let [nome "Ana" idade 25]
  (format "Oi, meu nome é %s e tenho %d anos." nome idade))
```

Isso produzirá a saída:

```
Oi, meu nome é Ana e tenho 25 anos.
```

## Imersão Profunda:

Na perspectiva histórica, Clojure não vem com suporte inerente para a interpolação de strings, em contraste com algumas outras linguagens de programação, como Python ou Ruby. Ao invés disso, utiliza-se o `format` (como no Java 'String.format()'), que é mais verboso mas fornece poderosas capacidades de formatação.

Como alternativa, você pode usar bibliotecas adicionais como `clojure.string/replace` que podem proporcionar uma experiência de interpolação de string mais elegante.

```Clojure
(require '[clojure.string :as str])

(let [nome "Ana" idade 25]
  (str/replace "Oi, meu nome é {nome} e tenho {idade} anos." 
               {"{nome}" nome "{idade}" idade}))
```

Em termos de detalhes de implementação, você gostaria de se familiarizar com os vários especificadores de formato no `format` do Clojure. Por exemplo, `%s` para strings, `%d` para integers e `%.nf` para números de ponto flutuante com 'n' dígitos após o ponto decimal.

## Veja Também:

- Documentação oficial Clojure para `format`: https://clojuredocs.org/clojure.core/format
- Guide to Clojure Strings: https://www.learn-clojure.com/clojure-guide-to-strings
- Biblioteca `clojure.string`: https://clojuredocs.org/clojure.string.