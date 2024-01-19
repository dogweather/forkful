---
title:                "Lendo argumentos de linha de comando"
html_title:           "Arduino: Lendo argumentos de linha de comando"
simple_title:         "Lendo argumentos de linha de comando"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

- Ler argumentos da linha de comando é uma forma com que programas interagem com o ambiente em que eles estão rodando.
- Programadores fazem isso para modificar o comportamento de um programa, baseando-se em parâmetros de entrada definidos.

## Como fazer:

Elm é uma linguagem de programação que roda no navegador, não no sistema operacional. Portanto, não tem acesso direto aos argumentos da linha de comando. No entanto, nós podemos simular este processo usando flags ao iniciar um aplicativo Elm.

```elm
import Browser
import Html exposing (text)
import Json.Decode as Decode

main =
  Browser.sandbox { init = init, update = update, view = view }

type alias Model = 
    { input : String }

init : Model
init input =
  { input = input }

type Msg 
= DoNothing

update : Msg -> Model -> Model
update msg model =
  model 

view : Model -> Html.Html Msg
view model =
  text model.input
```

Na linha de comando, você pode iniciar seu aplicativo Elm com argumentos como este:

```shell
elm make Main.elm --optimize --output=main.js
```

## Visão Profunda

- Os argumentos de linha de comando existem desde os primórdios da computação. Isso permitiu que programas fossem versionados e reutilizáveis.
- Como alternativa aos argumentos da linha de comando, algumas linguagens têm variáveis de ambiente para passar informações para um programa.
- No Elm, as flags são usadas para simular a leitura de argumentos da linha de comando. Entretanto, essa é uma limitação do Elm por ser executado no navegador e, não no sistema operacional.

## Veja também

- [Documentação oficial do Elm](https://guide.elm-lang.org/)
- [Fórum Elm para discussões](https://discourse.elm-lang.org/)
- [StackOverflow Elm para perguntas específicas](https://stackoverflow.com/questions/tagged/elm)