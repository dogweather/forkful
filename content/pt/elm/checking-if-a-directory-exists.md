---
title:                "Elm: Verificando se um diretório existe"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Por que verificar se um diretório existe no Elm?

Se você é um programador Elm, provavelmente já se deparou com a necessidade de verificar se um determinado diretório existe. Embora essa seja uma tarefa simples, pode ser de grande importância para garantir o bom funcionamento do seu código.

# Como fazer isso

Existem várias maneiras de verificar se um diretório existe no Elm. Aqui, mostraremos duas abordagens diferentes usando funções nativas do Elm.

## Usando a função `Directory.exists`

Uma das formas mais simples de verificar se um diretório existe é usando a função `Directory.exists` do pacote `elm/file`. Essa função retorna um `Task` que pode ser mapeado para `True` se o diretório existir ou `False` se não existir.

```
import File exposing (exists)
import Task exposing (map)

main : Task Never Bool
main = 
    exists "caminho/do/diretorio"
        |> map (\exists -> 
            if exists then True 
            else False)
```

No exemplo acima, utilizamos a função `exists` passando como argumento o caminho do diretório que queremos verificar. Em seguida, utilizamos a função `map` para mapear o resultado do `Task` para `True` ou `False` dependendo do resultado. 

## Usando a função `Directory.list`

Outra forma de verificar se um diretório existe é usando a função `Directory.list` do mesmo pacote. Essa função retorna um `Task` que pode ser mapeado para uma lista de arquivos e diretórios existentes no diretório especificado.

```
import File exposing (list)
import Task exposing (map)

main : Task Never (List String)
main = 
    list "caminho/do/diretorio"
        |> map (\files -> 
            if List.length files > 0 then True 
            else False)
```

Nesse caso, usamos a função `list` passando o caminho do diretório e mapeamos o resultado para uma lista de arquivos e diretórios existentes. Se essa lista tiver tamanho maior que 0, significa que o diretório existe.

# Detalhes sobre a verificação de diretórios

Tanto a função `Directory.exists` quanto a função `Directory.list` são baseadas na função `FileSystem.access`, que é a responsável por verificar se um determinado arquivo ou diretório existe. Por isso, se você estiver familiarizado com essa função, pode utilizá-la para realizar a verificação de diretórios de forma mais detalhada.

# Veja também

- Documentação oficial do pacote `elm/file`: https://package.elm-lang.org/packages/elm/file/latest/
- Função `FileSystem.access`: https://package.elm-lang.org/packages/elm/file/latest/FileSystem#access 
- Tutorial sobre manipulação de arquivos e diretórios no Elm: https://medium.com/@limadeveloper/manipulação-de-arquivos-e-diretórios-no-elm-1630d59adb96