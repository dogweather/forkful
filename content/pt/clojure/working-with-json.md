---
title:                "Trabalhando com JSON"
html_title:           "Clojure: Trabalhando com JSON"
simple_title:         "Trabalhando com JSON"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## Por que

Trabalhar com JSON (JavaScript Object Notation) é uma habilidade essencial para lidar com dados em aplicativos modernos. Com a popularidade do Clojure, é importante entender como esse idioma pode ser usado para manipular dados estruturados em formato JSON.

## Como Fazer

Para começar a trabalhar com JSON em Clojure, primeiro precisamos importar o pacote `clojure.data.json`, que fornece funções úteis para manipular e converter dados JSON.

```clojure
(ns meu-app.core
  (:require [clojure.data.json :as json]))
```

Para converter dados JSON em uma estrutura de dados Clojure, podemos usar a função `json/read-str`. Por exemplo, podemos converter os dados JSON abaixo em um mapa Clojure:

```clojure
(def dados "{\"nome\": \"João\", \"idade\": 30}")
(print (json/read-str dados))
```

A saída seria:

```
{"nome" "João", "idade" 30}
```

Além disso, podemos usar a função `json/write-str` para converter uma estrutura de dados Clojure em um formato JSON. Por exemplo, podemos converter o seguinte mapa em uma string JSON:

```clojure
(def mapa {"nome" "Maria", "idade" 25})
(print (json/write-str mapa))
```

A saída seria:

```
{"nome": "Maria", "idade": 25}
```

Podemos até mesmo converter estruturas de dados aninhadas. Por exemplo, podemos converter a seguinte lista em um formato JSON:

```clojure
(def lista ["amarelo" "vermelho" "azul"])
(print (json/write-str lista))
```

A saída seria:

```
["amarelo", "vermelho", "azul"]
```

## Profundidade

Além das funções `json/read-str` e `json/write-str`, o pacote `clojure.data.json` também fornece outras funções úteis para trabalhar com JSON, como `json/read` e `json/write`. Além disso, o pacote possui funções para manipular diferentes tipos de dados em formato JSON, como arrays, objetos e valores.

A documentação do pacote `clojure.data.json` oferece uma visão mais detalhada de todas as funções disponíveis para lidar com JSON em Clojure.

## Veja também

- Documentação do pacote `clojure.data.json`: https://clojure.github.io/data.json/
- Guia de referência rápida de Clojure para trabalhar com JSON: https://gist.github.com/jrmoran/a7c90a11d7c9d747cd4a55082110e708
- Exemplos práticos de como trabalhar com JSON em Clojure: https://github.com/matthiasn/clojure-json