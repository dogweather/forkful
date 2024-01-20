---
title:                "Lendo um arquivo de texto"
html_title:           "Bash: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Lendo um arquivo de texto em Elm

## O Que & Porquê?
Ler um arquivo de texto é o ato de acessar e ler conteúdo armazenado como texto em um arquivo. Os programadores fazem isso para obter dados de entrada, configurar parâmetros ou ler scripts para processamento.

## Como Fazer:
Infelizmente, devido à arquitetura do Elm (versão 0.19.1), não podemos ler diretamente arquivos em tempo real. A linguagem Elm é construída para segurança e roda no navegador, então não permitindo interações ao disco diretamente.

Porém, podemos simular a entrada de um arquivo de texto, copiando e colando seu conteúdo em um campo de entrada. Veja o exemplo abaixo:

```Elm
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

type alias Model = String

init : Model
init = ""

type Msg = NewContent String

update : Msg -> Model -> Model
update msg model =
  case msg of
    NewContent content -> content

view : Model -> Html Msg
view model = 
  Html.textarea 
   [ placeholder "Cole o conteúdo do arquivo aqui", 
     onInput NewContent 
   ] 
   [ text model ]

main = Html.beginnerProgram { model = init, view = view, update = update }
```

Este programa Elm simples possui um campo de entrada do tipo `textarea`. Quando você cola o conteúdo de um arquivo de texto nele, o modelo é atualizado e mostra o conteúdo do arquivo.

## Mergulho Profundo:
Elm foca na segurança. Portanto, ele é projetado para rodar em um ambiente de navegador com acesso limitado aos recursos globais do sistema operacional, como disco rígido. Essa é uma das razões pelas quais o Elm não permite a leitura direta de arquivos.

Uma alternativa seria usar JavaScript para ler o arquivo e, em seguida, passar os dados para o Elm através das Flags. Outra alternativa seria ter um backend para receber e manipular arquivos.

## Veja Também:
1. Documentação oficial do Elm: [https://elm-lang.org/docs](https://elm-lang.org/docs)
3. Arquivos e I/O em Elm na plataforma Ellie: [https://ellie-app.com/cZr5sbrVWNa1](https://ellie-app.com/cZr5sbrVWNa1)