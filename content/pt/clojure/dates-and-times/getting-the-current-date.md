---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:10.377251-07:00
description: "Obter a data atual na programa\xE7\xE3o \xE9 crucial por uma mir\xED\
  ade de raz\xF5es, incluindo o registro em log, marca\xE7\xE3o de tempo em eventos\
  \ e agendamento de tarefas.\u2026"
lastmod: '2024-03-13T22:44:46.207160-06:00'
model: gpt-4-0125-preview
summary: "Obter a data atual na programa\xE7\xE3o \xE9 crucial por uma mir\xEDade\
  \ de raz\xF5es, incluindo o registro em log, marca\xE7\xE3o de tempo em eventos\
  \ e agendamento de tarefas."
title: Obtendo a data atual
weight: 29
---

## O Que & Por Quê?
Obter a data atual na programação é crucial por uma miríade de razões, incluindo o registro em log, marcação de tempo em eventos e agendamento de tarefas. Em Clojure, um dialeto Lisp na JVM, essa tarefa aproveita as capacidades de interoperabilidade com Java, permitindo um acesso direto à rica API de Data e Hora do Java.

## Como Fazer:

### Usando Interoperabilidade com Java
A interoperabilidade sem emendas de Clojure com o Java permite que você acesse diretamente a API de Data e Hora do Java. Veja como você pode obter a data atual:

```clojure
(import java.time.LocalDate)

(defn get-current-date []
  (str (LocalDate/now)))

;; Saída de exemplo
(get-current-date) ; "2023-04-15"
```

### Usando a Biblioteca clj-time
Para uma solução mais idiomática em Clojure, você pode optar pela biblioteca `clj-time`, um wrapper em torno do Joda-Time, embora para a maioria dos novos projetos, a API de Data e Hora do Java 8 incorporada seja recomendada. No entanto, caso prefira ou necessite do `clj-time`:

Primeiro, adicione `clj-time` às dependências do seu projeto. No seu `project.clj`, inclua:

```clojure
[clj-time "0.15.2"]
```

Depois, use-o para obter a data atual:

```clojure
(require '[clj-time.core :as time])

(defn get-current-date-clj-time []
  (str (time/now)))

;; Saída de exemplo
(get-current-date-clj-time) ; "2023-04-15T12:34:56.789Z"
```

Ambos os métodos fornecem maneiras rápidas e eficazes de obter a data atual em Clojure, aproveitando o poder da plataforma Java subjacente ou a conveniência de uma biblioteca específica para Clojure.
