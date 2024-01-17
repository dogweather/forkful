---
title:                "Verificando se um diretório existe"
html_title:           "Elm: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O que e Por Que?

Verificar se um diretório existe é um processo comumente utilizado por programadores para confirmar a existência de uma pasta específica em um sistema de arquivos. Isso pode ser útil ao criar uma aplicação que precise acessar ou manipular arquivos armazenados em um diretório específico. 

## Como Fazer:

Para verificar se um diretório existe em Elm, podemos usar a função `File.isDirectory` que retorna um `Bool` indicando se o diretório especificado existe ou não. Por exemplo:

```Elm
import File
import Task

existeDiretorio : String -> Task x Bool
existeDiretorio diretorio =
    File.isDirectory diretorio
```

No exemplo acima, usamos a função `File.isDirectory` em conjunto com a função `Task` para criar uma tarefa que irá retornar um `Bool`. Podemos chamar essa tarefa usando a função `Task.attempt` e lidando com o resultado usando `Task.andThen` e `Task.onError`.

## Mergulho Profundo:

A função `File.isDirectory` foi adicionada à versão 0.19 do Elm e é uma alternativa à função `File.exists` que anteriormente era usada para verificar a existência de um diretório. Ao contrário de muitas outras linguagens de programação, Elm possui um sistema de arquivos virtual, o que significa que não é possível acessar diretamente o sistema de arquivos do computador em que o código está sendo executado. Em vez disso, o Elm usa o sistema de arquivos do usuário para simular o acesso ao sistema de arquivos e proteger a privacidade do usuário.

## Veja Também:

- Documentação oficial do Elm: [https://elm-lang.org/docs](https://elm-lang.org/docs)
- Como trabalhar com arquivos em Elm: [https://guide.elm-lang.org/effects/file.html](https://guide.elm-lang.org/effects/file.html)