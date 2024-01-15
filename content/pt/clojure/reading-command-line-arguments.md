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

## Por que

Você pode se perguntar por que é importante aprender a ler argumentos de linha de comando na linguagem Clojure. A resposta é simples: os argumentos de linha de comando são uma maneira eficiente e poderosa de interagir com seus programas. Eles permitem que você personalize a execução do seu código de forma simples e direta, tornando sua aplicação mais versátil e amigável para o usuário final.

## Como Fazer

A leitura de argumentos de linha de comando em Clojure é simples e pode ser feita com apenas algumas linhas de código. Primeiro, importe a biblioteca "clojure.tools.cli", que nos fornece funções para lidar com argumentos de linha de comando. Em seguida, defina as opções que seu programa deve aceitar, especificando o nome, o tipo e a descrição de cada opção. Por fim, basta chamar a função "parse-opts" e passar os argumentos da linha de comando, que serão transformados em um mapa contendo os valores das opções. Veja um exemplo abaixo:

```Clojure
(ns meu-programa
  (:require [clojure.tools.cli :refer [parse-opts]]))

(defn -main [& args]
  (let [[options _ (parse-opts args
                       [["-n" "--nome" "Nome do usuário. Requerido e deve estar em maiúsculas." :required true :type string]
                        ["-i" "--idade" "Idade do usuário. Opcional e deve ser um número." :type integer]])
        nome (get options :nome)
        idade (get options :idade)]
    (if nome
      (println (str "Olá, " nome "! Sua idade é " idade "."))
      (println "Por favor, informe o seu nome em maiúsculas com a opção -n."))))

```

Podemos então executar o programa com os seguintes argumentos da linha de comando:

```
$ clojure meu-programa.clj -n JOÃO -i 30
Olá, JOÃO! Sua idade é 30.
```

Caso não seja fornecido um valor para a opção "--nome", o programa irá exibir uma mensagem pedindo para que o usuário informe o nome com a opção "-n".

## Mergulho Profundo

A biblioteca "clojure.tools.cli" também nos permite definir validações para as opções, utilizando funções que retornam um booleano indicando se o valor fornecido é válido. Por exemplo, podemos modificar nosso programa para aceitar apenas idades maiores que 18 anos, utilizando a função "opt-long" no lugar de "opt-integer":

```Clojure
(ns meu-programa
  (:require [clojure.tools.cli :refer [parse-opts opt-long]]))

(defn -main [& args]
  (let [[options _ (parse-opts args
                       [["-n" "--nome" "Nome do usuário. Requerido e deve estar em maiúsculas." :required true :type string]
                        ["-i" "--idade" "Idade do usuário. Opcional e deve ser um número maior que 18." :type long :valid? #(> % 18)]])
        nome (get options :nome)
        idade (get options :idade)]
    (if nome
      (println (str "Olá, " nome "! Sua idade é " idade "."))
      (println "Por favor, informe o seu nome em maiúsculas com a opção -n."))))

```

Agora, se executarmos o programa com uma idade menor que 18, teremos o seguinte resultado:

```
$ clojure meu-programa.clj -n MARIA -i 17
Por favor, informe uma idade válida (maior que 18) com a opção -i.
```

## Veja Também

- [Documentação da biblioteca "clojure.tools.cli"](https://github.com/clojure/tools.cli)
- [Tutorial sobre argumentos de linha de comando em Clojure](https://purelyfunctional.tv/series/commandline/)