---
date: 2024-01-20 17:59:29.490386-07:00
description: "Enviar uma solicita\xE7\xE3o HTTP \xE9 como fazer uma pergunta a um\
  \ site e esperar uma resposta. Programadores fazem isso para interagir com APIs,\
  \ coletar dados ou\u2026"
lastmod: '2024-02-25T18:49:43.855164-07:00'
model: gpt-4-1106-preview
summary: "Enviar uma solicita\xE7\xE3o HTTP \xE9 como fazer uma pergunta a um site\
  \ e esperar uma resposta. Programadores fazem isso para interagir com APIs, coletar\
  \ dados ou\u2026"
title: "Enviando uma requisi\xE7\xE3o HTTP"
---

{{< edit_this_page >}}

## O Quê e Por Quê?
Enviar uma solicitação HTTP é como fazer uma pergunta a um site e esperar uma resposta. Programadores fazem isso para interagir com APIs, coletar dados ou comunicar com outros serviços web.

## Como Fazer:
Primeiro, adicione a biblioteca [clj-http](https://github.com/dakrone/clj-http) ao seu projeto edn:

```clojure
:dependencies [[clj-http "3.12.3"]]
```

Depois, esquente os dedos e code:

```clojure
(require '[clj-http.client :as client])

;; GET simples
(let [response (client/get "http://httpbin.org/get")]
  (println (:status response))
  (println (:headers response))
  (println (:body response)))

;; POST com dados
(let [response (client/post "http://httpbin.org/post" {:form-params {:foo "bar"}})]
  (println (:status response))
  (println (:body response)))
```

Isso vai imprimir o status da resposta, cabeçalhos, e o corpo da mensagem.

## Imersão:
Lá no começo, HTTP era só um meio de buscar documentos. Hoje, é a espinha dorsal da web. 

Alternativas modernas incluem GraphQL ou WebSockets para necessidades mais complexas ou em tempo real.

Detalhes da implementação: Usar clj-http é simples, mas tem suas profundezas. Por baixo dos panos, clj-http usa a biblioteca Apache HttpComponents para fazer a conexão e processar a resposta. Pode ser bloqueante ou assíncrono, dependendo de como você usa.

## Veja Também:
- [Documentação clj-http](https://github.com/dakrone/clj-http)
- Tutorial HTTP da Mozilla para um entendimento sólido do protocolo: https://developer.mozilla.org/en-US/docs/Web/HTTP
- [Clojure para os curiosos](https://www.braveclojure.com/): Pra quem quer se aprofundar em Clojure.
