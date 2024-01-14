---
title:    "Clojure: Imprimindo saída de depuração"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# Por que imprimir saída de depuração em Clojure?

Depurar código é uma parte essencial do processo de programação, e imprimir saída de depuração pode ser uma ferramenta valiosa nesse processo. Ao imprimir informações relevantes durante a execução do código, é possível entender melhor o fluxo e o estado do programa, o que pode ajudar a identificar e corrigir possíveis erros.

# Como fazer?

A impressão de saída de depuração em Clojure é bastante simples e pode ser feita usando a função `prn`. Vamos dar uma olhada em alguns exemplos de como usar essa função:

```
;; Imprimir uma string
(prn "Olá, mundo!")

;; Imprimir uma variável
(def numero 7)
(prn "O número é" numero)

;; Imprimir o resultado de uma operação matemática
(prn (+ 3 4))

;; Imprimir o conteúdo de uma coleção
(def nomes ["Maria" "João" "Ana"])
(prn "A segunda pessoa é" (nth nomes 1))

;; Imprimir o estado de uma estrutura de dados
(def meu-mapa {:nome "Carla" :idade 26})
(prn "O nome é" (:nome meu-mapa) "e a idade é" (:idade meu-mapa))
```

A saída da execução desses exemplos será:

```
"Olá, mundo!"
"O número é" 7
7
"A segunda pessoa é" "João"
"O nome é" "Carla" "e a idade é" 26
```

# Mergulho profundo

Além da função `prn`, também é comum utilizar a função `println` para imprimir saída de depuração em Clojure. A diferença entre elas é que `println` adiciona uma quebra de linha ao final da impressão, enquanto `prn` não o faz. Além disso, ambas podem receber múltiplos parâmetros e imprimir todos eles separados por espaço.

Também é possível usar o `println` e o `prn` dentro de uma expressão `when` para imprimir apenas quando uma determinada condição é atendida. Isso pode ser útil para evitar que a saída de depuração polua o resultado final do programa.

# Veja também

- [Documentação oficial do Clojure](https://clojure.org/)
- [Tutorial de Clojure para iniciantes do DevMedia](https://www.devmedia.com.br/introducao-ao-clojure/26152)
- [Tutorial de Clojure para programadores Java do Caelum](https://www.caelum.com.br/apostila-java-orientacao-objetos/programando-em-clojure/#12-10-exercicios)