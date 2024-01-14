---
title:                "Clojure: Convertendo uma string para minúsculas"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Por que
## Por que converter uma string para caixa baixa

Converter uma string para caixa baixa pode ser útil ao trabalhar com dados de entrada que possuem inconsistências de capitalização, garantindo que o código trate todas as strings de forma uniforme.

## Como fazer
Para converter uma string para caixa baixa em Clojure, podemos usar a função "lower-case" da biblioteca padrão "clojure.string". Veja o exemplo abaixo:

```Clojure
(ns exemplo)

(require '[clojure.string :as str])

(def texto "Olá, MUndo!")

(str/lower-case texto)

;; Output: "olá, mundo!"
```

## Mergulho profundo
Ao usar a função "lower-case", é importante ter em mente que ela não modifica a string original, apenas retorna uma nova string em caixa baixa. Além disso, a função usa as regras de caixa baixa definidas pelo idioma em que o código está sendo executado.

Outra opção para converter uma string para caixa baixa é usar a função "lower-case*" da biblioteca "clojure/data.string". Essa função é mais flexível, pois permite especificar o idioma desejado. Veja o exemplo abaixo:

```Clojure
(ns exemplo)

(require '[clojure.data.string :as data-str])

(def texto "Bonjour, Le Monde!")

(data-str/lower-case* texto :locale "fr")

;; Output: "bonjour, le monde!"
```

# Veja também
- Documentação da função "lower-case" da biblioteca padrão "clojure.string": https://clojuredocs.org/clojure.string/lower-case
- Documentação da função "lower-case*" da biblioteca "clojure/data.string": https://clojuredocs.org/clojure.data/string/lower-case*