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

# Checando se um Diretório Existe em Elm

## O Que & Por Quê?

Verificar se um diretório existe é um passo necessário para garantir que os programas acessem o local certo para ler ou gravar dados. Os programadores fazem isso para evitar erros causados pela tentativa de acessar diretórios inexistentes.

## Como Fazer:

Infelizmente, no atual estado da linguagem Elm (0.19.1), não é possível interagir diretamente com o sistema de arquivos de uma maneira que permitiria verificar se um diretório existe, pois o Elm é uma linguagem voltada principalmente para a web e o navegador. Aqui está um exemplo de como você pode ter feito isso se fosse possível:

```Elm
-- Isto é apenas um exemplo fictício, 'Dir.exists' não existe na linguagem Elm atual
exemploFicticio : String -> Task String Bool
exemploFicticio path =
    Dir.exists path
        |> Task.attempt
            (\result ->
               case result of
                    Ok exists -> exists
                    Err _ -> False
            )
```

## Aprofundando

Historicamente, a linguagem Elm sempre se manteve leal à sua filosofia de foco na segurança, simplicidade e facilidade para o desenvolvimento web. Isto explica por que a linguagem não fornece uma maneira direta de se interagir com o sistema de arquivos, pois essa ação pode ser vulnerável a ataques do tipo Directory Traversal e outras violações de segurança.

No entanto, existem algumas soluções alternativas. Poderíamos usar `ports` para se comunicar com código JavaScript, que tem permissão para usar a API `fs` do Node.js (ambiente do lado do servidor). Lembre-se de que é importante que você esteja ciente das implicações de segurança ao usar este método.

Quanto aos detalhes de implementação, a verificação da existência de um diretório geralmente envolve a utilização de uma API do sistema operacional que lista diretórios e arquivos, em seguida, procurando pelo diretório desejado no resultado retornado. Como já mencionado, na linguagem de programação Elm, isso é feito através de bibliotecas JavaScript interfaceadas via `ports`.

## Veja Também

- [Documentação oficial de Elm](https://elm-lang.org/docs)
- [Guia Elm para Interoperabilidade JavaScript](https://guide.elm-lang.org/interop/)
- [Documentação da API Node.js FS](https://nodejs.org/api/fs.html)