---
title:                "Baixando uma página da web"
html_title:           "Elm: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O que & Porque?

Baixar uma página da web é o processo de transferir um documento HTML de um servidor para um dispositivo, permitindo que o usuário visualize o conteúdo do site em seu navegador. Os programadores geralmente fazem isso para acessar informações de sites externos ou adicionar funcionalidades a seus próprios aplicativos.

## Como:

Para baixar uma página da web usando Elm, você pode usar a função `Http.get` e passar a URL desejada como argumento. Aqui está um exemplo de código:

````Elm
import Http

Http.get "https://www.example.com" 
    |> Task.perform PageLoaded FailedToLoad
````

Neste exemplo, estamos usando a função `get` da biblioteca padrão `Http` e passando a URL "https://www.example.com" como argumento. Em seguida, usamos o operador de pipeline `|>` para encadear a função `Task.perform` que irá lidar com o resultado da solicitação. Se a solicitação for bem-sucedida, a função `PageLoaded` será executada com o conteúdo da página como argumento. Caso contrário, a função `FailedToLoad` será executada com uma mensagem de erro.

## Deep Dive:

O processo de baixar páginas da web é uma parte essencial do desenvolvimento web e é usado em uma variedade de aplicações, como aplicativos de busca, agregadores de notícias e muito mais. Além disso, as funções `Http.get` podem aceitar argumentos adicionais, como headers e parâmetros, para permitir uma personalização ainda maior na solicitação.

Alternativas para baixar páginas da web em Elm incluem o uso da extensão `port` para se comunicar com JavaScript ou a utilização de outras bibliotecas de terceiros que ofereçam mais funcionalidades para lidar com solicitações HTTP.

## See Also:

- Documentação oficial de Elm sobre funções Http.get: https://package.elm-lang.org/packages/elm/http/latest/Http#get
- Tutorial sobre como baixar páginas da web em Elm: https://dev.to/elmuniversity/downloading-web-pages-with-elm-2eni