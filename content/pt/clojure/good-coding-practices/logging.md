---
aliases:
- /pt/clojure/logging/
date: 2024-01-26 01:02:30.966188-07:00
description: "O log \xE9 essencialmente o equivalente de software ao di\xE1rio de\
  \ bordo de um navio; \xE9 uma maneira de registrar eventos que acontecem enquanto\
  \ uma aplica\xE7\xE3o\u2026"
lastmod: 2024-02-18 23:08:57.808992
model: gpt-4-1106-preview
summary: "O log \xE9 essencialmente o equivalente de software ao di\xE1rio de bordo\
  \ de um navio; \xE9 uma maneira de registrar eventos que acontecem enquanto uma\
  \ aplica\xE7\xE3o\u2026"
title: Registro de Logs
---

{{< edit_this_page >}}

## O Que e Por Quê?
O log é essencialmente o equivalente de software ao diário de bordo de um navio; é uma maneira de registrar eventos que acontecem enquanto uma aplicação está em execução. Programadores fazem isso para manter um registro desses eventos para depuração, trilhas de auditoria ou para obter insights sobre o comportamento de um sistema em produção.

## Como fazer:
Clojure se apoia nas facilidades de log do Java, mas você pode acessá-las de uma maneira mais idiomática de Clojure. Vamos dar uma olhada em como você pode usar `clojure.tools.logging`, que oferece uma abstração simples sobre vários frameworks de log:

Primeiro, adicione uma dependência para `clojure.tools.logging` e uma implementação de log como `log4j` no seu `project.clj`:

```clojure
:dependencies [[org.clojure/clojure "1.10.3"]
               [org.clojure/tools.logging "1.1.0"]
               [log4j/log4j "1.2.17"]]
```
Agora, vamos registrar algumas mensagens:

```clojure
(require '[clojure.tools.logging :as log])

(defn compute-answer-to-everything []
  (log/debug "Iniciando um cálculo intenso...")
  (Thread/sleep 3000) ; Simulação de um longo cálculo
  (log/info "Cálculo concluído. A resposta é 42.")
  42)

(compute-answer-to-everything)
```
A saída não mostrará mensagens `DEBUG` por padrão, já que os níveis de log são normalmente configurados para `INFO`:

```
INFO  [seu-namespace] - Cálculo concluído. A resposta é 42.
```

Você pode configurar os níveis de log e os appenders em um arquivo `log4j.properties` para obter uma saída mais verbosa, se necessário.

## Mergulho Profundo
`clojure.tools.logging` do Clojure existe há algum tempo e serve como uma ponte entre o código Clojure e o mundo de log do Java. Historicamente, o Java já passou por várias iterações e bibliotecas para registro de logs, como a API de log embutida do Java, `log4j`, `slf4j`, e `logback`.

No Clojure, enquanto você pode usar diretamente os frameworks de log do Java, `clojure.tools.logging` detecta e delega ao framework de log que encontrar em seu classpath, evitando que você fique estritamente acoplado a uma implementação específica. Isso pode ajudar a manter seu código Clojure mais portátil e modular.

Alternativas ao `clojure.tools.logging` dentro do ecossistema Clojure incluem bibliotecas como `timbre`, que é uma biblioteca de log puramente Clojure com recursos como rotação de logs, filtragem e logging assíncrono já inclusos.

Detalhes de implementação são cruciais quando se trata de registro de logs em um ambiente com múltiplas threads como o Clojure. Aqui, imutabilidade e o gerenciamento de efeitos colaterais oferecem vantagens distintas. O registro de logs, como um efeito colateral, deve ser tratado com cuidado para evitar gargalos de desempenho e garantir a segurança entre as threads, algo que a maioria dos frameworks de log do Java já cuida.

Por último, considere o uso de log estruturado, onde os logs são escritos como dados estruturados (como JSON). Isso pode ser extremamente útil para análises e processamentos posteriores, particularmente ao lidar com sistemas distribuídos em larga escala.

## Veja Também
Se você quer saber mais, considere consultar estes recursos:

- Documentação do Clojure Tools Logging: https://github.com/clojure/tools.logging
- Timbre, uma biblioteca de log Clojure: https://github.com/ptaoussanis/timbre
- Configurando Log4J no Clojure: http://clojure-doc.org/articles/tutorials/logging_with_log4j.html
- Manual do Logback para configurações avançadas: http://logback.qos.ch/manual/
- Um guia sobre log estruturado em Clojure: https://corfield.org/blog/2020/04/28/structured-logging/
