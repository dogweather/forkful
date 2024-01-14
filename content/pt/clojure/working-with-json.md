---
title:                "Clojure: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/working-with-json.md"
---

{{< edit_this_page >}}

Por que trabalhar com JSON em Clojure?

JSON (JavaScript Object Notation) é um formato de troca de dados muito comum em aplicações web modernas. Ele permite que dados estruturados sejam transmitidos de forma leve e legível. Em Clojure, o uso de JSON pode ser extremamente útil para comunicação com APIs ou para armazenamento de dados. Neste artigo, exploraremos como trabalhar com JSON em Clojure e como aproveitar ao máximo essa poderosa ferramenta.

Como fazer:

Para trabalhar com JSON em Clojure, é necessário primeiro importar a biblioteca "clj-json" em seu projeto. Você pode fazer isso adicionando a seguinte dependência ao seu arquivo "project.clj":

```
:dependencies [[clj-json "0.5.3"]]
```

Agora, vamos supor que temos o seguinte JSON em uma API que desejamos acessar:

```
{ "nome": "Maria", "idade": 25, "altura": 1.65 }
```

Podemos converter esse JSON em uma estrutura de dados em Clojure usando a função "json/read-str":

```
(require '[clj-json.core :as json])
(def dados (json/read-str "{ \"nome\": \"Maria\", \"idade\": 25, \"altura\": 1.65 }"))
```

Podemos então acessar os valores do JSON como se fossem chaves em um map:

```
(:nome dados) ; retorna "Maria"
(:idade dados) ; retorna 25
(:altura dados) ; retorna 1.65
```

Da mesma forma, podemos criar um novo JSON a partir de uma estrutura de dados em Clojure usando a função "json/write-str":

```
(def novo-json (json/write-str {:nome "João" :idade 30 :altura 1.75}))
```

O valor de "novo-json" será uma string contendo o seguinte JSON:

```
{ "nome": "João", "idade": 30, "altura": 1.75 }
```

Esses exemplos são apenas uma pequena amostra do que é possível fazer com a biblioteca "clj-json". Com ela, você pode facilmente transformar dados entre JSON e Clojure, o que é extremamente útil para comunicação com APIs ou armazenamento de dados em seu projeto.

Aprofundando:

Além das funções mencionadas, a biblioteca "clj-json" oferece muitas outras opções e recursos. Você pode, por exemplo, definir um formato personalizado para a conversão de datas ou usar anotações de teclas para mapear chaves de JSON para chaves em Clojure. Para mais informações, consulte a documentação oficial da biblioteca em: https://clj-json.net/.

Veja também:

- Documentação oficial da biblioteca "clj-json": https://clj-json.net/
- Artigo sobre manipulação de JSON em Clojure: https://www.braveclojure.com/read-and-write-json/
- Tutorial sobre comunicação com API em Clojure usando JSON: https://luminusweb.com/docs/api.html