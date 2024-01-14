---
title:                "Clojure: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando em Clojure?

Ler argumentos da linha de comando pode ser útil para criar programas interativos ou para processar dados de entrada externos. Em Clojure, isso é feito facilmente usando a função `clojure.core/command-line-args`.

## Como fazer:

Para começar, precisamos importar a função `command-line-args` do namespace `clojure.core`. Em seguida, podemos usar a função dentro de um bloco de código delimitado por ```Clojure ... ``` para ler os argumentos passados na linha de comando.

Exemplo de código:

```Clojure
(ns meu-programa
  (:require [clojure.core :refer [command-line-args]]))
  
(def argumentos-da-linha-de-comando (command-line-args))

(print argumentos-da-linha-de-comando)
```

Exemplo de saída:

```
$ leitura-de-argumentos.clj primeiro-argumento segundo-argumento

["primeiro-argumento" "segundo-argumento"]
```

Podemos também usar a função `get` para acessar argumentos específicos passando seu índice como parâmetro. Por exemplo:

```Clojure
(print (get argumentos-da-linha-de-comando 0))
(print (get argumentos-da-linha-de-comando 1))
```

Exemplo de saída:

```
$ leitura-de-argumentos.clj primeiro-argumento segundo-argumento

"primeiro-argumento"
"segundo-argumento"
```

## Mergulho profundo:

Além de ler argumentos passados na linha de comando, também podemos especificar argumentos com valores na forma de um vetor. Por exemplo:

```Clojure
(def argumentos-e-valores [["primeiro" "argumento"] ["segundo" "argumento"]])

(print (get (get argumentos-e-valores "primeiro") 1))
(print (get (get argumentos-e-valores "segundo") 1))
```

Exemplo de saída:

```
"argumento"
"argumento"
```

Também podemos usar a função `assoc` para adicionar novos argumentos e valores ao vetor. Por exemplo:

```Clojure
(def argumentos-e-valores (assoc argumentos-e-valores ["terceiro" "argumento"] ["quarto" "argumento"]))

(print (get (get argumentos-e-valores "terceiro") 1))
(print (get (get argumentos-e-valores "quarto") 1))
```

Exemplo de saída:

```
"argumento"
"argumento"
```

## Veja também:

- [Documentação oficial do Clojure sobre command-line-args](https://clojuredocs.org/clojure.core/command-line-args)
- [Tutorial em vídeo sobre leitura de argumentos da linha de comando em Clojure (em inglês)](https://www.youtube.com/watch?v=XHIrC3DUBdM)
- [Exemplos práticos de uso de command-line-args em projetos em Clojure (em inglês)](https://github.com/search?q=command-line-args+language%3Aclojure&type=Repositories)