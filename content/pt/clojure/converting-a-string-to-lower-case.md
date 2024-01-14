---
title:                "Clojure: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Por que

Converter uma string para letras minúsculas é uma tarefa comum em muitos projetos de programação. Isso pode ser útil para validar entradas de usuário, pesquisar em uma lista ou simplesmente padronizar o formato dos dados em um programa.

# Como fazer

Em Clojure, a função `clojure.string/lower-case` pode ser usada para converter uma string para letras minúsculas. Veja um exemplo abaixo:

```Clojure 
(def x "Exemplo String")
(clojure.string/lower-case x)
```
A saída desse código será `"exemplo string"`, com todas as letras minúsculas.

# Mergulho profundo

Ao converter uma string para letras minúsculas, é importante considerar a diferença de acentuação entre diferentes idiomas. Por exemplo, em português, a letra "I" maiúscula com acento agudo é diferente da letra "i" minúscula. Para garantir que a conversão seja feita corretamente, é necessário utilizar a função `java.lang.String/toLowerCase` que leva em consideração essas diferenças.

Além disso, é importante notar que a função `lower-case` retorna uma nova string, deixando a string original intacta. Se você deseja modificar a string original, é necessário utilizar a função `string/lower-case!` que altera a string original in-place.

# Veja também

- [Documentação oficial da função `clojure.string/lower-case`](https://clojuredocs.org/clojure.string/lower-case)
- [Post sobre conversão de strings para capitalização em Clojure](https://www.hugodias.com.br/conversao-de-string-para-maiuscula-minuscula-e-capitalizada-em-clojure/)

---

# Veja também