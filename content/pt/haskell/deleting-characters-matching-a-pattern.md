---
title:    "Haskell: Excluindo caracteres que correspondem a um padrão"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

##Por que?

Quando estamos trabalhando com grandes volumes de dados ou realizando tarefas repetitivas em nosso código Haskell, é comum nos depararmos com a necessidade de excluir determinados caracteres que correspondem a um determinado padrão. Nesses casos, a utilização de funções específicas para essa tarefa pode nos ajudar a otimizar nosso código e tornar nosso trabalho mais eficiente.

##Como Fazer

Para excluir caracteres correspondentes a um padrão em Haskell, podemos utilizar a função `deleteBy` do módulo `Data.List`. Essa função recebe como parâmetros o predicado que define o padrão a ser correspondido e a lista na qual os caracteres serão excluídos. Vamos ver um exemplo:

```Haskell
import Data.List

-- criando uma lista de strings
lista = ["abc1", "abc2", "abc3", "def1", "def2"]

-- excluindo as strings que começam com "abc"
novaLista = deleteBy (\x y -> x `startsWith` y) "abc" lista
```

No exemplo acima, utilizamos a função `deleteBy` com uma função anônima que compara se o início de cada string é igual a "abc". O resultado da função será a nova lista `["def1", "def2"]`, pois as strings "abc1", "abc2" e "abc3" serão excluídas.

## Mergulho Mais Profundo

Além da função `deleteBy`, existem outras maneiras de excluir caracteres correspondentes a um padrão em Haskell. Podemos utilizar funções como `filter` e `dropWhile` do módulo `Data.List`, ou mesmo criar nossas próprias funções personalizadas.

Além disso, é importante entender que a função `deleteBy` não modifica a lista original, mas sim retorna uma nova lista com as exclusões realizadas. Portanto, é importante atribuir o resultado da função a uma nova variável ou utilizá-lo de alguma forma em nosso código.

## Veja Também

- [Documentação da função `deleteBy`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#v:deleteBy)
- [Outras funções úteis para manipulação de listas em Haskell](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html)