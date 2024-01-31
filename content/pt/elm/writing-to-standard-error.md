---
title:                "Escrevendo no erro padrão"
date:                  2024-01-19
simple_title:         "Escrevendo no erro padrão"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Escrever no erro padrão (standard error) é uma forma de enviar mensagens de erro ou diagnóstico separadas da saída principal do programa. Programadores usam isso para comunicar falhas, problemas e informações de debug sem interferir nos dados de saída.

## How to:
Elm é uma linguagem funcional que roda no navegador e, não tendo um stderr tradicional, lida com erros de maneira diferente. O conceito mais próximo seria enviar mensagens de erro através do sistema de mensagens para o JavaScript usando `ports`. Eis um exemplo simplista.

```Elm
port module Main exposing (..)

-- Definindo port para enviar mensagens para o JavaScript
port error : String -> Cmd msg

-- Simulando um erro e enviando a mensagem
simulateError : Cmd msg
simulateError =
    error "Algo deu errado!"

-- Na parte do JavaScript, você teria algo assim:
// Suponde que você tem `app` que é sua aplicação Elm iniciada.
app.ports.error.subscribe(function(message) {
    console.error(message);
});
```
Saída esperada no console JavaScript seria:
```
Algo deu errado!
```

## Deep Dive
Elm mantém as coisas no lado seguro e, por isso, não interage diretamente com operações de I/O como a escrita de stderr típica de outras linguagens. No início, Elm focava em evitar efeitos colaterais diretos, levando a padrões alternativos de tratamento de erros, como a utilização de `Maybe` e `Result` types para lidar com erros de maneira mais funcional. Ports permitem que Elm "converse" com JavaScript para operações fora do seu escopo direto.

## See Also
- Documentação oficial sobre Ports em Elm: [https://guide.elm-lang.org/interop/ports.html](https://guide.elm-lang.org/interop/ports.html)
- Tratamento de erros em Elm utilizando `Result`: [https://package.elm-lang.org/packages/elm/core/latest/Result](https://package.elm-lang.org/packages/elm/core/latest/Result)
- Discussão sobre estratégias de erro em Elm no Elm Discourse: [https://discourse.elm-lang.org/](https://discourse.elm-lang.org/)
