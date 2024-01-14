---
title:    "Clojure: Lendo argumentos da linha de comando"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que

Em muitas linguagens de programação, é comum ter que lidar com argumentos de linha de comando, ou seja, parâmetros passados para o programa na hora de executá-lo. No Clojure não é diferente e entender como ler esses argumentos é essencial para construir aplicações eficientes e flexíveis.

## Como Fazer

O Clojure possui uma função nativa chamada "command-line-args" que retorna uma lista contendo todos os argumentos passados na linha de comando. Vamos dar uma olhada em um exemplo de como podemos utilizá-la:

```Clojure
(def args (command-line-args))
(println "O primeiro argumento é:" (first args))
(println "O segundo argumento é:" (second args))
```

Ao executar esse código com os seguintes argumentos na linha de comando:

```
java -jar meu_programa.jar arg1 arg2
```

O output será:

```
O primeiro argumento é: arg1
O segundo argumento é: arg2
```

Podemos também utilizar a função "get-in" para acessar argumentos específicos passados com flags, por exemplo:

```Clojure
(def args (command-line-args))
(println "O argumento da flag -l é:" (get-in args [1]))
```

Ao executarmos o programa com o seguinte comando:

```
java -jar meu_programa.jar -l valor_da_flag
```

O output será:

```
O argumento da flag -l é: valor_da_flag
```

## Mergulho Profundo

Além da função "command-line-args", o Clojure também possui a biblioteca "tools.cli" que oferece mais recursos para lidar com argumentos de linha de comando. Ela inclui funções para definir flags, opções de ajuda e validação dos argumentos passados. Para utilizá-la, basta adicionar a dependência no seu projeto e seguir a documentação.

## Veja Também

- [Documentação oficial do Clojure sobre "command-line-args"](https://clojuredocs.org/clojure.core/command-line-args)
- [Documentação da biblioteca "tools.cli"](https://github.com/clojure/tools.cli)