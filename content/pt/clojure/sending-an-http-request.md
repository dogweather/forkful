---
title:                "Enviando uma solicitação http"
html_title:           "Clojure: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

Por que usar Clojure para enviar requisições HTTP?

Existem diversas linguagens de programação que permitem enviar requisições HTTP, mas Clojure é uma opção popular entre os programadores devido às suas funcionalidades que facilitam o trabalho com requisições e respostas de APIs. Além disso, a linguagem é conhecida por ser concisa e ter uma sintaxe simples, o que torna o processo de enviar requisições ainda mais eficiente.

Como fazer:

Para enviar uma requisição HTTP com Clojure, é necessário utilizar a biblioteca "clj-http". Primeiro, é preciso configurar o namespace para importar as funções necessárias:

```Clojure
(ns app.core
  (:require [clj-http.client :as client]))
```

Em seguida, podemos usar a função "client/get" para enviar uma requisição GET para um determinado URL, e armazenar a resposta em uma variável:

```Clojure
(def response (client/get "https://api.github.com/users/clojure/repos"))
```

Podemos então acessar o conteúdo da resposta utilizando a função "client/content":

```Clojure
(def content (client/content response))
```

O conteúdo deve ser retornado como uma string, que pode ser facilmente transformada em dados utilizáveis usando a função "clojure.edn/read-string":

```Clojure
(def data (clojure.edn/read-string content))
```

Agora podemos acessar as informações da resposta, como por exemplo, o nome de um dos repositórios retornados:

```Clojure
(println (str "O nome do repositório é: " (-> data first :full_name)))
```

Saída do código acima:
```
O nome do repositório é: clojure/tools.nrepl
```

Deep Dive:

A biblioteca "clj-http" oferece diversas outras funções úteis para trabalhar com requisições HTTP, como por exemplo, "client/post" para enviar requisições POST e "client/put" para requisições PUT. Além disso, é possível adicionar headers, parâmetros e corpo de requisição utilizando as opções disponíveis nas funções.

Veja também:

- Documentação da biblioteca "clj-http": https://github.com/dakrone/clj-http
- Exemplo de código para requisições HTTP em Clojure: https://gist.github.com/momo-lab/3bdf9754cd5bce3093850c8113d4d33a