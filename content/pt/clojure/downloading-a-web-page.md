---
title:                "Baixando uma página da web"
html_title:           "Clojure: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por que

Baixar páginas da web pode ser uma tarefa útil e interessante para muitas pessoas. Pode ser necessário para fazer cópias de segurança de sites importantes, ou pode ser uma forma de extrair dados de uma página para uso posterior. Independentemente do motivo, é importante saber como realizar essa tarefa em Clojure.

## Como fazer

```Clojure
(require '[clj-http.client :as client])
(require '[clojure.java.io :as io])

; Definindo a URL da página que queremos baixar
(def url "https://www.example.com")

; Usando a biblioteca clj-http para realizar a requisição HTTP
(def response (client/get url))

; Escrevendo o conteúdo da página em um arquivo local
(io/copy (io/input-stream (:body response)) (io/file "pagina.html"))

; Imprimindo o status da resposta HTTP
(println (:status response))

; Imprimindo o conteúdo da página
(println (:body response))
```

Saída do console ao executar o código acima:

```
200
<!doctype html>
...

Com o código acima, realizamos uma requisição HTTP para a URL especificada e salvamos o conteúdo da página em um arquivo local. Também imprimimos o código de status e o conteúdo da página na saída do console.

## Mergulho Profundo

Existem diferentes abordagens para baixar páginas da web em Clojure, como usar outras bibliotecas além da clj-http, ou até mesmo escrever sua própria função para fazer a solicitação HTTP. É importante também considerar a segurança ao realizar essas requisições, evitando ataques de força bruta ou sobrecarga de servidores.

## Veja também

- [Documentação oficial da biblioteca clj-http](https://clj-http.github.io/)
- [Tutorial de Clojure na página do Loop Infinito](https://www.linux.ime.usp.br/~lucasmmg/clojure(pt_BR).pdf)
- [Artigo sobre segurança em requisições HTTP com Clojure](https://www.infoq.com/br/news/2017/05/seguranca-clj-http/)