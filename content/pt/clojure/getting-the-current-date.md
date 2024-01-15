---
title:                "Obtendo a data atual"
html_title:           "Clojure: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por quê?

Você já se perguntou como os aplicativos e sistemas sabem a data atual? Talvez você esteja planejando criar um aplicativo que precisa desta informação para funcionar corretamente. Ou talvez você esteja apenas curioso sobre como isso é feito. Em qualquer caso, aprender a obter a data atual em Clojure pode ser uma habilidade útil e interessante.

## Como fazer

```Clojure
(require '[clojure.java-time :as time])

(def date (time/local-date))
(println "Data atual:" date)

;; Saída esperada:
;; "Data atual: #inst "2021-07-19T00:00:00.000-00:00"
```

O código acima utiliza a biblioteca `java-time` para importar a função `local-date`, que retorna a data atual no formato de instância `#inst`. Este formato específico é semelhante ao utilizado pelo sistema Java para representar datas e horários.

Também é possível obter a data e hora atual no mesmo objeto, utilizando a função `local-date-time`:

```Clojure
(def datetime (time/local-date-time))
(println "Data e hora atual:" datetime)

;; Saída esperada:
;; "Data e hora atual: #inst "2021-07-19T12:00:00.000-00:00"
```

Se você quiser personalizar o formato da data e hora de saída, pode utilizar a função `zoned-date-time` e adicionar um padrão de formatação como argumento:

```Clojure
(def custom-format (time/zoned-date-time "yyyy/MM/dd HH:mm"))
(println "Data e hora atual no formato personalizado:" custom-format)

;; Saída esperada:
;; "Data e hora atual no formato personalizado: 2021/07/19 12:00"
```

Lembre-se de sempre utilizar a diretiva `:require` para importar a biblioteca `java-time` antes de utilizar suas funções.

## Deep Dive

Embora a biblioteca `java-time` seja uma opção popular para manipular datas e horas em Clojure, ela ainda não é considerada parte da biblioteca padrão da linguagem. Portanto, algumas funções e padrões podem mudar de tempos em tempos e é importante manter-se atualizado sobre as melhores práticas.

Uma das vantagens de utilizar a biblioteca `java-time` em Clojure é a capacidade de combinar seus recursos com funções nativas da linguagem, como `map`, `filter` e `reduce`. Isso permite realizar operações poderosas e eficientes com datas e horas.

Outra vantagem é a possibilidade de utilizar a função `between` para calcular a diferença entre duas datas ou horas. Por exemplo, se você quiser saber quantos dias faltam até o seu aniversário, pode utilizar o seguinte código:

```Clojure
(def my-birthday #inst "2021-10-03T00:00:00.000-00:00") ; data do seu aniversário
(def today (time/local-date)) ; data atual
(def days-left (time/between today my-birthday :days)) ; calcula a diferença em dias

(println "Faltam" days-left "dias até o meu aniversário!")

;; Saída esperada:
;; "Faltam 75 dias até o meu aniversário!"
```

Este é apenas um exemplo das possibilidades que a biblioteca `java-time` oferece. É importante explorar suas funções e experimentar para entender melhor como ela funciona.

## Veja também

- [Documentação da biblioteca java-time](https://github.com/juxt/java-time)
- [Funções nativas de data e hora em Clojure](https://clojuredocs.org/clojure.java-time)
- [Vídeo tutorial sobre a biblioteca java-time em Clojure](https://www.youtube.com/watch?v=xrM0wTbUJYY)