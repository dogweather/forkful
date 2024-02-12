---
title:                "Removendo aspas de uma string"
aliases:
- /pt/clojure/removing-quotes-from-a-string/
date:                  2024-01-26T03:38:48.693893-07:00
model:                 gpt-4-0125-preview
simple_title:         "Removendo aspas de uma string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Remover aspas de uma string significa se livrar daqueles caracteres de aspas duplas ou simples que englobam seu texto. Programadores fazem isso para limpar dados, garantir uniformidade, ou preparar strings para processamento onde as aspas são indesejadas ou podem causar erros.

## Como fazer:
Em Clojure, strings são imutáveis, então quando falamos sobre "remover aspas", estamos realmente falando sobre criar uma nova string sem aspas. Aqui está a essência usando `clojure.string/replace`:

```clojure
(require '[clojure.string :as str])

; Vamos nos livrar dessas aspas duplas
(defn remove-double-quotes [s]
  (str/replace s #"\"" ""))

; E expulsar as aspas simples
(defn remove-single-quotes [s]
  (str/replace s #"\'" ""))

; Uso de exemplo:
(remove-double-quotes "\"Olá, Mundo!\"") ; => "Olá, Mundo!"
(remove-single-quotes "'Olá, Mundo!'")   ; => "Olá, Mundo!"
```
Quer lidar com as aspas simples e duplas de uma só vez? Dê uma olhada nisso:

```clojure
(defn remove-quotes [s]
  (str/replace s #"[\"\']" ""))

; Uso de exemplo:
(remove-quotes "\"Olá, 'Clojure' Mundo!\"") ; => "Olá, Clojure Mundo!"
```

## Aprofundamento
Nos velhos tempos, quando os dados eram mais bagunçados que o quarto de uma criança, as aspas em strings eram a norma para denotar texto. Mas conforme a ciência da computação evoluiu, as aspas se tornaram mais do que apenas delimitadores de texto — elas assumiram papéis sintáticos nas linguagens de programação.

Clojure, com sua herança Lisp, não usa aspas da mesma maneira que algumas outras linguagens poderiam usar. Com certeza, elas são usadas para denotar strings, mas também têm um papel especial na criação de literais. Independente disso, remover aspas de strings continua sendo uma tarefa atemporal.

Por que não apenas cortar as extremidades de uma string? Bem, isso é supor que suas aspas estão sempre abraçando o começo e o fim da sua string como um par de avós excessivamente afetuosos. Dados do mundo real são mais bagunçados. Entre as expressões regulares (regex), que permitem você mirar aquelas aspas independentemente de onde elas estejam escondidas.

Alternativas? Claro, você pode se aventurar com `subs`, `trim`, `triml`, `trimr`, ou até mesmo transdutores se quiser se exibir. Mas `replace` com regex é como levar um sabre de luz para uma briga de facas — vai direto ao ponto.

## Veja Também
Se seu cérebro está ávido por mais bondades de manipulação de strings em Clojure, essas migalhas podem ajudar:

- ClojureDocs sobre `clojure.string/replace`: https://clojuredocs.org/clojure.string/replace
- Expressões regulares em Clojure: https://clojure.org/guides/learn/syntax#_regex
- Interoperabilidade com Java para manipulação de strings (afinal, Clojure roda na JVM): https://clojure.org/reference/java_interop#_working_with_strings

Não pare apenas em remover aspas. Há um mundo inteiro de magia de strings lá fora em Clojure esperando para ser descoberto.
