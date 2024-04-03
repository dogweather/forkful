---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:50.328758-07:00
description: "Escrever para o erro padr\xE3o (stderr) consiste em direcionar mensagens\
  \ de erro e diagn\xF3sticos para o stream stderr, separando-os da sa\xEDda padr\xE3\
  o (stdout).\u2026"
lastmod: '2024-03-13T22:44:46.212865-06:00'
model: gpt-4-0125-preview
summary: "Escrever para o erro padr\xE3o (stderr) consiste em direcionar mensagens\
  \ de erro e diagn\xF3sticos para o stream stderr, separando-os da sa\xEDda padr\xE3\
  o (stdout)."
title: "Escrevendo para o erro padr\xE3o"
weight: 25
---

## O Que & Por Quê?
Escrever para o erro padrão (stderr) consiste em direcionar mensagens de erro e diagnósticos para o stream stderr, separando-os da saída padrão (stdout). Programadores fazem isso para diferenciar a saída regular do programa das mensagens de erro, permitindo um depuração e registro de logs mais eficazes.

## Como fazer:
Em Clojure, você pode escrever no stderr usando o stream `*err*`. Aqui está um exemplo básico:

```clojure
(.write *err* "Esta é uma mensagem de erro.\n")
```

Note que, após escrever uma mensagem, você deve limpar o stream para garantir que a mensagem seja imediatamente enviada:

```clojure
(flush)
```

Exemplo de saída para stderr:
```
Esta é uma mensagem de erro.
```

Se você está lidando com exceções, talvez queira imprimir os rastreamentos de pilha no stderr. Use `printStackTrace` para isso:

```clojure
(try
  ;; Código que pode lançar uma exceção
  (/ 1 0)
  (catch Exception e
    (.printStackTrace e *err*)))
```

Para um registro de erros mais estruturado, bibliotecas de terceiros como `timbre` podem ser configuradas para registrar no stderr. Aqui está uma configuração básica e seu uso:

Primeiro, adicione `timbre` às suas dependências. Depois, configure-o para usar o stderr:

```clojure
(require '[taoensso.timbre :as timbre])

(timbre/set-config! [:appenders :standard-out :enabled?] false) ;; Desativa o registro de logs no stdout
(timbre/set-config! [:appenders :spit :enabled?] false) ;; Desativa o registro de logs em arquivo
(timbre/set-config! [:appenders :stderr :min-level] :error) ;; Habilita o stderr para erros

(timbre/error "Ocorreu um erro durante o processamento da sua solicitação.")
```

Isso direcionará mensagens de nível de erro para o stderr, tornando-as distintas da saída padrão da aplicação.
