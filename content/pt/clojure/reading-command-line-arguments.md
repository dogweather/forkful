---
title:    "Clojure: Lendo argumentos da linha de comando"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Por que ler argumentos de linha de comando é importante
Ler argumentos de linha de comando é uma habilidade valiosa para programadores que desejam criar aplicativos interativos e personalizáveis. Ao saber como ler argumentos de linha de comando, você pode permitir que seus usuários modifiquem o comportamento do seu programa de acordo com suas necessidades e preferências.

## Como ler argumentos de linha de comando em Clojure
Ler argumentos de linha de comando em Clojure é bastante simples. Primeiro, você precisa importar a biblioteca `clojure.core`, que possui uma função útil para lidar com argumentos de linha de comando, chamada `command-line-args`. Em seguida, você pode usá-la em seu código para acessar e processar os argumentos passados durante a execução do programa. Veja um exemplo abaixo:

```Clojure
(ns meu-programa
  (:require [clojure.core :refer [command-line-args]]))

(defn -main []
  (let [args (command-line-args)]
    (println "Os argumentos passados foram:" args)))
```

Se você executar o código acima com o seguinte comando no terminal:

`clojure -m meu-programa arg1 arg2 arg3`

O resultado será:

`Os argumentos passados foram: ["arg1" "arg2" "arg3"]`

## Mergulhando mais fundo
Além de ler argumentos passados pelo usuário, é possível limitar o número de argumentos que seu programa aceita e definir valores padrão para aqueles que não forem fornecidos. Para fazer isso, a função `command-line-args` aceita dois parâmetros: `:summary` e `:default`, que podem ser usados da seguinte forma:

```Clojure
(ns meu-programa
  (:require [clojure.core :refer [command-line-args]]))

(defn -main []
  (let [sumario "Este programa aceita três argumentos: nome, idade e cidade."
        args (command-line-args {:summary sumario :default ["João" "25" "Rio de Janeiro"]})]
    (println "Olá," (first args) "! Tenho" (second args) "anos e moro em" (last args) ".")))
```

Caso o usuário não forneça nenhum argumento, o programa usará os valores padrão especificados. No exemplo acima, a saída seria:

`Olá, João! Tenho 25 anos e moro em Rio de Janeiro.`

## Veja também
- [Documentação da função `command-line-args`](https://clojuredocs.org/clojure.core/command-line-args)
- [Exemplos de leitura de argumentos de linha de comando em Clojure](https://gist.github.com/mperham/3808499)