---
title:    "Clojure: Excluindo caracteres que correspondam a um padrão"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

### Por que

Neste artigo, vamos explorar como deletar caracteres que correspondem a um padrão em programas escritos em Clojure. Isso pode ser útil para remover espaços em branco, pontuação ou outros caracteres indesejados de strings.

## Como fazer

Para deletar caracteres correspondentes a um padrão em Clojure, utilizamos a função `clojure.string/replace` combinada com expressões regulares. Por exemplo, para remover todos os espaços em branco de uma string, podemos usar o seguinte código:

```Clojure
(clojure.string/replace "Este é um exemplo de string com espaços" #"\s+" "")
```
A saída seria: "Esteéumexemplodestringcomespaços".

Aqui, a função `replace` recebe três argumentos: a string original, a expressão regular correspondendo ao padrão que queremos deletar (nesse caso, qualquer espaço em branco representado pela regex `s+`) e a string vazia que será usada para substituir os caracteres encontrados.

Podemos também utilizar grupos de captura em nossas expressões regulares para substituir apenas parte dos caracteres correspondentes. Por exemplo, podemos usar a regex `[#"'`]+` para remover todos os símbolos de pontuação de uma string:

```Clojure
(clojure.string/replace "Essa é uma frase com muitas! Pontuações?'?" #"[#"'`]+" "")
```

A saída seria: "Essa é uma frase com muitas Pontuações".

## Deep Dive

A função `replace` é parte da biblioteca padrão de Clojure e pode ser encontrada no namespace `clojure.string`. Ela aceita strings, chars ou seqs como entrada e pode ser usada para substituir um único caractere ou múltiplos caracteres correspondentes a um padrão.

Um detalhe importante é que a função `replace` não modifica a string original, mas retorna uma nova string com as substituições realizadas. Portanto, é necessário salvar essa nova string em uma variável ou imprimir sua saída para visualizar o resultado.

Para saber mais sobre expressões regulares em Clojure e como utilizá-las em conjunto com a função `replace`, confira a documentação oficial e os links da seção "Veja Também" abaixo.

## Veja Também

- Documentação oficial de Clojure sobre a função `replace`: https://clojuredocs.org/clojure.string/replace
- Tutorial sobre expressões regulares em Clojure: https://purelyfunctional.tv/lesson/regular-expressions-in-clojure/
- Mais exemplos e exercícios práticos sobre substituição de caracteres em strings em Clojure: https://www.4clojure.com/problem/109