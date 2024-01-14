---
title:                "Clojure: Obtendo a data atual"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Por que obter a data atual em Clojure?

A obtenção da data atual é uma tarefa comum em muitos programas, especialmente quando se trabalha com dados temporais. Em Clojure, existem várias maneiras de obter a data atual, o que pode ser útil para uma variedade de aplicações. Neste post, exploraremos como podemos obter a data atual em Clojure e alguns detalhes mais avançados sobre o assunto.

## Como obter a data atual em Clojure

Existem duas maneiras principais de obter a data atual em Clojure: usando funções da biblioteca padrão e usando a biblioteca externa `clj-time`.

### Usando funções da biblioteca padrão

Clojure inclui uma variedade de funções úteis para trabalhar com datas e horários, incluindo `inst`, `now` e `local-date-time`. Aqui está um exemplo de como podemos usá-las para obter a data atual:

```
Clojure
(def inst-now (inst)) ; cria uma instância com a data e hora atuais
(def now (now)) ; retorna um objeto de data e hora atual
(def local-time (local-date-time)) ; retorna um objeto com data e hora local
```

### Usando a biblioteca `clj-time`

A biblioteca externa `clj-time` é uma ótima opção para trabalhar com datas e horários em Clojure. Aqui está como podemos usá-la para obter a data atual:

```
Clojure
(require '[clj-time.core :as time]) ; importa a biblioteca clj-time
(def now (time/now)) ; retorna uma data e hora atual a partir do fuso horário do sistema
(def national-now (time/now "America/Sao_Paulo")) ; retorna uma data e hora atual baseada no fuso horário especificado
```

## Profundidade: Mais informações sobre a obtenção da data atual

Ao trabalhar com datas e horários em Clojure, é importante entender como a biblioteca lida com fusos horários e como converter objetos de data e hora para diferentes formatos. É também útil conhecer as diferenças entre as funções `inst` e `now`, que retornam resultados ligeiramente diferentes.

Para obter mais informações sobre esses tópicos, recomendamos a leitura da documentação oficial do Clojure.

## Veja também

- Documentação oficial do Clojure sobre datas e horários: https://clojuredocs.org/clojure.java-time

- Documentação oficial do Clojure sobre a biblioteca `clj-time`: https://github.com/clj-time/clj-time

- Exemplos práticos de uso da biblioteca `clj-time`: https://www.baeldung.com/clojure-clj-time

Esperamos que este post tenha sido útil para entender como obter a data atual em Clojure e como trabalhar com datas e horários em geral. Boas codificações!