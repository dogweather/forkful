---
title:                "Convertendo uma data em uma string"
date:                  2024-01-20T17:36:33.977152-07:00
model:                 gpt-4-1106-preview
simple_title:         "Convertendo uma data em uma string"

category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O que é & Por quê?

Converter uma data em uma string permite que você a formate para visualização ou armazenamento. Os programadores frequentemente fazem isso para padronizar a representação de datas, facilitar a comparação, ou simplesmente exibir uma data de uma maneira legível para usuários.

## Como fazer:

**Conversão básica de data para string:**

```Clojure
(require '[java.time.format :as fmt])
(require '[java.time :as time])

(let [hoje (time/LocalDate/now)
      formato (fmt/DateTimeFormatter/ofPattern "dd/MM/yyyy")]
  (.format hoje formato))
```

**Saída:**

```
"31/03/2023"
```

**Usando clj-time para mais flexibilidade:**

```Clojure
(require '[clj-time.format :as f])

(let [hoje (time/LocalDate/now)
      formatador (f/formatter "dd-MM-yyyy")]
  (f/unparse formatador hoje))
```

**Saída:**

```
"31-03-2023"
```

## Aprofundamento

Na época do Java 7 e anterior, a manipulação de datas em Clojure era um tanto verbosa, pois dependíamos diretamente da API `java.util.Date`. Com a chegada do Java 8, a API de data/hora foi completamente reformulada com `java.time`, a qual Clojure abraçou para oferecer uma experiência mais suave.

Alternativas, como a biblioteca `clj-time`, são baseadas em Joda-Time e oferecem uma API ainda mais rica e idiomatic para Clojure. Por outro lado, `java.time` é suficiente para muitos casos de uso e não requer dependências adicionais.

Detalhes de implementação como fuso horário (timezone) e localidade (locale) podem afetar o resultado da conversão de datas. Por isso, é importante definir estes parâmetros se você precisa de consistência através de diferentes ambientes ou localizações. 

## Veja também

- Documentação oficial do `clj-time`: [https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time)
- Guia do `java.time`: [https://clojure.org/guides/deps_and_cli](https://clojure.org/guides/deps_and_cli)
- Página de referência da API DateTimeFormatter: [https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
