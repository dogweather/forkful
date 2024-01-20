---
title:                "Baixando uma página da web"
html_title:           "Bash: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Baixando Uma Página Web com Clojure

## O Que & Por Que?
Baixar uma página web é o ato de recuperar e armazenar o conteúdo de uma página da internet. Os programadores fazem isso para analisar ou manipular dados, testar aplicativos ou simplesmente para criar uma cópia offline.

## Como Fazer:
Vamos usar a biblioteca `clj-http` para recuperar a página.

Primeiro, instale a biblioteca adicionando a seguinte dependência ao seu `project.clj`:

```Clojure
[clj-http "3.12.1"]
```

Aqui está um exemplo de como baixar uma página:
```Clojure
(ns hello-web.core
 (:require [clj-http.client :as client]))

(defn fetch-page [url]
(let [response (client/get url {:as :auto})]
(println (:status response))
(println (:headers response))
(println (:body response))))
```

Quando executado, este código imprimirá o status da resposta, os cabeçalhos e o corpo (ou conteúdo) da página.

## Uma Visão Mais Profunda

### Contexto Histórico
O protocolo HTTP foi criado na década de 1990 para transferência de hipertexto. Hoje, é usado para muitos outros propósitos, inclusive para baixar páginas web. A biblioteca clj-http, que estou usando neste exemplo, é uma das bibliotecas mais completas para fazer solicitações HTTP em Clojure.

### Alternativas
Além de `clj-http`, existem algumas bibliotecas alternativas que você pode usar para baixar páginas da web com Clojure, como `http-kit` e `aleph`.

### Detalhes de Implementação
A função `client/get` do `clj-http` envia uma solicitação GET para o URL especificado e retorna uma resposta. Estamos usando o argumento `{:as :auto}`, que instrui a biblioteca a tentar detectar automaticamente o formato de resposta e convertê-lo de forma apropriada.

## Veja Também

1. [Documentação do clj-http](https://github.com/dakrone/clj-http)
3. [Detalhes técnicos sobre o protocolo HTTP](https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Overview)

Feliz programação em Clojure!