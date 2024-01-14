---
title:    "Clojure: Convertendo uma string em minúsculas"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

##Por que converter uma string para minúsculo?

Mas antes de discutirmos como converter uma string para minúsculo em Clojure, vamos primeiro entender o motivo de se fazer isso. Converter uma string para minúsculo pode ser útil em situações onde é necessário comparar duas strings sem levar em conta a diferença entre maiúsculas e minúsculas. Também pode ser útil para padronizar a entrada de dados em um programa, garantindo que todas as strings estejam no mesmo formato.

##Como fazer

Em Clojure, existem várias formas de converter uma string para minúsculo. A maneira mais simples é utilizando a função `lower-case` do namespace `clojure.string`. Veja um exemplo:

```Clojure
(require '[clojure.string :as str])
(str/lower-case "Olá Mundo") ; output: "olá mundo"
```

Outra opção é utilizar a função `lower-case` do namespace `java.lang.String`. Esta função é mais rápida do que a função `clojure.string/lower-case`, mas é necessário converter a string para um objeto `java.lang.String` antes de chamá-la. Veja o exemplo:

```Clojure
(.toLowerCase "Olá Mundo") ; output: "olá mundo"
```

Por fim, se você estiver trabalhando com strings muito grandes e não quiser alocar memória extra na conversão, pode utilizar a função `char-utils/lower-case-ascii` do namespace `org.apache.commons.lang3`. Esta função trabalha diretamente com os valores ASCII dos caracteres, evitando a criação de novas strings. Veja o exemplo:

```Clojure
(require '[clojure.core :as c]
         '[org.apache.commons.lang3 :as utils])
(utils/lower-case-ascii (c/str "Olá Mundo")) ; output: "olá mundo"
```

##Aprofundando-se

Agora que vimos como converter uma string para minúsculo em Clojure, é importante entender que esta conversão é sensível ao idioma do sistema operacional em que o código está sendo executado. Isso significa que, se o sistema operacional estiver configurado para um idioma diferente do português, a conversão para minúsculo pode não funcionar corretamente. Nesses casos, é recomendado utilizar a função `Character.toLowerCase` da linguagem Java, que é independente do idioma do sistema operacional.

##Veja também

- Documentação da função `lower-case` do `clojure.string`: https://clojuredocs.org/clojure.string/lower-case
- Documentação da função `.toLowerCase` do `java.lang.String`: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--
- Documentação da função `lower-case-ascii` do `org.apache.commons.lang3`: https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/CharUtils.html#lowerCase%EF%BC%88char%EF%BC%89