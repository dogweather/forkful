---
title:                "Lendo argumentos da linha de comando"
html_title:           "Clojure: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# O que e por que?

Ler argumentos da linha de comando é uma maneira que os programadores têm de obter informação a partir da entrada do usuário que vem diretamente da linha de comando. Isso é útil porque permite que os programas sejam executados de diferentes maneiras, dependendo dos argumentos fornecidos pelo usuário.

# Como fazer:

```Clojure
(def args (command-line-args))
(prn "O primeiro argumento é:" (first args))
(prn "O segundo argumento é:" (second args))
```

Saída:

```Clojure
"O primeiro argumento é: argumento1"
"O segundo argumento é: argumento2"
```

# Profundidade do assunto:

Ler argumentos da linha de comando é uma prática comum em linguagens de programação, como uma forma de interação com o usuário e personalização da execução do programa. Antes do uso da linha de comando, os programas geralmente eram executados com parâmetros definidos previamente no código.

Existem alternativas para a leitura de argumentos da linha de comando, como por exemplo, a leitura de um arquivo de configuração externo. A escolha entre as diferentes opções depende do contexto e do objetivo do programa.

Em Clojure, a função `command-line-args` retorna uma lista com os argumentos fornecidos pelo usuário na linha de comando. É importante lembrar que esses argumentos são sempre interpretados como strings, então a conversão para outros tipos de dados pode ser necessária.

# Veja também:

- A documentação oficial da função `command-line-args`: https://clojuredocs.org/clojure.core/command-line-args
- Um exemplo prático de como utilizar a leitura de argumentos da linha de comando em Clojure: https://dzone.com/articles/cli-arguments-in-clojure-the-straightforward-way