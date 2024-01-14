---
title:    "Clojure: Buscando e substituindo texto"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Por que

Muitas vezes, quando estamos escrevendo código, precisamos fazer mudanças rápidas e sistemáticas em um trecho de texto. É aí que a funcionalidade de busca e substituição se torna essencial. Neste artigo, vamos aprender como usar essa funcionalidade em Clojure e como ela pode facilitar nossas tarefas de programação.

## Como fazer

Para fazer busca e substituição em Clojure, podemos usar a função `clojure.string/replace` que recebe três argumentos: a string original, o que queremos procurar e o que queremos substituir. Vejamos alguns exemplos de como usar essa função:

```Clojure
(clojure.string/replace "Olá, mundo!" "mundo" "Portugal")
;; saída: "Olá, Portugal!"

(clojure.string/replace "Gosto de programar em Clojure" "e" "ar")
;; saída: "Garstar de programar em Clarjar"

(clojure.string/replace "Fico ansioso pelo próximo release." "ansioso" "empolgado")
;; saída:"Fico empolgado pelo próximo release."
```

Além disso, podemos usar expressões regulares como argumento de busca. Por exemplo, se quisermos substituir todas as vogais em uma string por um ponto de exclamação, podemos fazer o seguinte:

```Clojure
(clojure.string/replace "O que você está fazendo?" #"[aeiou]" "!")
;; saída: "O q!! v!cê !stá f!!z!nd!!"
```

É importante lembrar que a função `replace` retorna uma cópia da string original com as alterações desejadas. Ela não modifica a string original, portanto, é necessário armazenar o resultado em uma nova variável.

## Aprofundando

A função `replace` pode ser usada não apenas em strings, mas também em qualquer tipo de sequência, como vetores e listas. Também podemos usar outras funções de Clojure para filtrar e mapear os elementos antes de fazer a substituição. Por exemplo, se tivermos uma lista de nomes e quisermos adicionar um sobrenome a todos eles, podemos fazer o seguinte:

```Clojure
(def nomes ["Ana" "João" "Carlos"])

(map #(str % " Silva") nomes)
;; saída: ("Ana Silva" "João Silva" "Carlos Silva")
```

Podemos combinar essa técnica com a função `replace` para adicionar o sobrenome apenas aos nomes que começam com a letra "A":

```Clojure
(map #(str % " Silva") (filter #(.startsWith % "A") nomes))
;; saída: ("Ana Silva")
```

Também é possível usar a função `replace` para remover caracteres indesejados, como espaços em branco, de uma string. Por exemplo, se tivermos a seguinte string: "Eu gosto de programar em Clojure", e quisermos remover todos os espaços em branco, podemos fazer o seguinte:

```Clojure
(clojure.string/replace "Eu gosto de programar em Clojure" #"\s" "")
;; saída: "EugostodeprogramaremClojure"
```

## Veja também

- Documentação oficial de `clojure.string/replace`: https://clojure.github.io/clojure/clojure.string-api.html#clojure.string/replace
- Tutorial sobre expressões regulares em Clojure: https://clojuredocs.org/clojure.core/re-seq
- Outras funções úteis de manipulação de strings em Clojure: https://clojuredocs.org/clojure.string