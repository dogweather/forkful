---
title:                "Elm: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe?

Ao programar em Elm, é importante saber se um diretório existe antes de tentar acessá-lo ou manipulá-lo. Caso contrário, o programa pode falhar. Neste post, vamos explorar como verificar se um diretório existe em Elm e por que isso é uma boa prática.

## Como fazer?

Para verificar se um diretório existe em Elm, podemos usar a função `exists` do módulo `Directory`. Esta função recebe como argumento o caminho do diretório que queremos verificar e retorna um `Task` contendo um booleano que indica se o diretório existe ou não.

 ```Elm
 Directory.exists "/caminho/do/diretorio"
    |> Task.perform Trivial
```
Este código irá imprimir `True` se o diretório existir e `False` se não existir.

## Profundamente

Mas como a função `exists` funciona por trás dos panos? Ela utiliza a API do sistema operacional para verificar se o diretório existe. Se o sistema operacional não suportar esse tipo de consulta, a função sempre retornará `False`. Por isso, é importante verificar se o sistema operacional é compatível com a operação antes de usar a função.

## Veja também

Confira a documentação oficial do módulo `Directory` para saber mais sobre a função `exists`. Você também pode se interessar por esses outros links:

- [Tutorial de Elm](https://guide.elm-lang.org/)
- [Documentação oficial do Elm](https://elm-lang.org/docs)
- [Fórum da comunidade de Elm](https://discourse.elm-lang.org/)