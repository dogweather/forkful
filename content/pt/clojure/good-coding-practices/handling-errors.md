---
date: 2024-01-26 00:51:46.156712-07:00
description: "Como fazer: Clojure, assim como seus ancestrais Lisp, depende de exce\xE7\
  \xF5es para lidar com erros. Aqui est\xE1 como voc\xEA mostra o que pode fazer quando\
  \ as\u2026"
lastmod: '2024-03-13T22:44:46.204121-06:00'
model: gpt-4-1106-preview
summary: "Clojure, assim como seus ancestrais Lisp, depende de exce\xE7\xF5es para\
  \ lidar com erros."
title: Tratamento de erros
weight: 16
---

## Como fazer:
Clojure, assim como seus ancestrais Lisp, depende de exceções para lidar com erros. Aqui está como você mostra o que pode fazer quando as coisas dão errado.

Lançar uma exceção é direto ao ponto:
```Clojure
(throw (Exception. "Ops! Algo deu errado."))
```

Capturando uma exceção, você fará isso bastante:
```Clojure
(try
  ;; código arriscado
  (/ 1 0)
  (catch ArithmeticException e
    (println "Não é possível dividir por zero!"))
  ;; bloco finally executa de qualquer maneira
  (finally 
    (println "Código de limpeza vai aqui.")))
```
Saída de exemplo para o bloco catch acima:
```
Não é possível dividir por zero!
Código de limpeza vai aqui.
```

Usando `ex-info` e `ex-data` para um contexto mais rico sobre exceções:
```Clojure
(try
  ;; causando uma exceção personalizada
  (throw (ex-info "Erro personalizado" {:type :custom-failure}))
  (catch Exception e
    ;; obtendo os dados de nossa exceção personalizada
    (println (ex-data e))))
```
Saída de exemplo:
```
{:type :custom-failure}
```

## Aprofundamento
A história do tratamento de erros em Clojure não é radicalmente diferente de outros Lisps ou até mesmo Java (do qual herda o mecanismo `try-catch`). É pragmático; usar exceções é o caminho principal, assim como no Java, mas Clojure oferece um sabor funcional com `ex-info` e `ex-data` para dados de erro mais ricos.

Alternativas para tratamento de erros em Clojure incluem o uso de construções monádicas, como a monada `either` de bibliotecas como `cats`, ou core.async para propagação de erros baseada em canais. No entanto, essas são mais complexas e usadas em cenários específicos.

Historicamente, o tratamento de erros em linguagens de programação evoluiu de simples retornos de status para os mecanismos de tratamento de exceção mais sofisticados das linguagens modernas. Clojure opta pela simplicidade e um toque de programação funcional, misturando o antigo e o novo.

## Veja Também
- Guia de Clojure para exceções: https://clojure.org/guides/exceptions
- Biblioteca “Cats” para abordagens mais funcionais: https://github.com/funcool/cats
- “Core.async” para programação assíncrona: https://github.com/clojure/core.async
