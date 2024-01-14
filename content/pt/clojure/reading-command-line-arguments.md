---
title:                "Clojure: Lendo argumentos da linha de comando"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando em Clojure?

Ler argumentos da linha de comando é uma habilidade importante para programadores de Clojure, pois permite que você crie programas interativos e flexíveis. Além disso, ao ler argumentos da linha de comando, você pode acessar informações específicas do usuário e personalizar a execução do seu programa.

## Como fazer

Para ler argumentos da linha de comando em Clojure, você pode usar a função `command-line-opts` do namespace `clojure.core`. Essa função retorna uma lista com todos os argumentos passados na linha de comando.

Aqui está um exemplo de código que lê dois argumentos da linha de comando e os imprime na tela usando a função `println`:

```Clojure
(ns meu-programa
  (:require [clojure.core :refer [command-line-opts]]))

(defn -main []
  (let [argumentos (command-line-opts)]
    (println "O primeiro argumento é:" (nth argumentos 0))
    (println "O segundo argumento é:" (nth argumentos 1))))
```

Suponha que você tenha salvo este código em um arquivo chamado `meu-programa.clj` e queira executá-lo com os argumentos "olá" e "mundo". Você pode fazer isso executando o seguinte comando no seu terminal:

```bash
clj meu-programa.clj olá mundo
```

O código acima irá imprimir na tela:

```bash
O primeiro argumento é: olá
O segundo argumento é: mundo
```

## Mergulho profundo

Além de usar a função `command-line-opts`, também é possível definir argumentos específicos que seu programa irá ler ao ser executado. Isso pode ser feito usando a biblioteca `tools.cli`. Para isso, você pode seguir os seguintes passos:

1. Adicionar `[org.clojure/tools.cli "0.4.0"]` como dependência em seu arquivo `project.clj`.

2. Importar a biblioteca no seu código: `(:require [clojure.tools.cli :refer [parse-opts]])`.

3. Definir argumentos com a função `defcli`, onde você pode especificar opções, como nome, descrição e tipo de argumento.

4. Usar a função `parse-opts` para ler os argumentos e executar sua lógica de acordo.

Veja um exemplo de código usando a biblioteca `tools.cli`:

```Clojure
(ns meu-programa
  (:require [clojure.tools.cli :refer [defcli parse-opts]]))

(defcli programa
  "Este é um programa de exemplo."
  ["-n" "--nome"
   "Nome do usuário."
   :default "Desconhecido"
   :argname "NOME"]
  ["-i" "--idade"
   "Idade do usuário."
   :default 0
   :argname "IDADE"])

(defn -main [& args]
  (let [{:keys [nome idade]} (parse-opts args)]
    (println "Olá," nome)
    (println "Você tem" idade "anos.")))
```

Ao executar o comando `clj meu-programa.clj -n João -i 30`, o código acima irá imprimir na tela:

```bash
Olá, João
Você tem 30 anos.
```

## Veja também

- [Documentação da função `command-line-opts`](https://clojuredocs.org/clojure.core/command-line-opts)
- [Documentação da biblioteca `tools.cli`](https://github.com/clojure/tools.cli)