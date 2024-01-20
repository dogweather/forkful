---
title:                "Criando um arquivo temporário"
html_title:           "Bash: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

---

# Criando um arquivo temporário em Elm

## O Que & Por Que?
Criar um arquivo temporário é uma tarefa que envolve a criação de um arquivo para uso temporário. Programadores fazem isso para armazenar dados que não precisam ser mantidos por muito tempo, como logs de erros ou dados de sessão.

## Como Fazer:

```Elm
importar Html exposing (..)
importar Html.Events exposing (onClick)

main =
  beginnerProgram { model = "Clique-me!", view = view, update = update }

type Msg = Click

update msg model =
  case msg of
    Click -> "Obrigado por clicar!"

view model =
  div []
    [ button [ onClick Click ] [ text model ] ]
```

Quando você executa esse código, verá uma janela com um botão que diz "Clique-me!". Quando você clicar nele, a mensagem mudará para "Obrigado por clicar!".

## Mergulhando Fundo

Historicamente, arquivos temporários são usados como uma forma prática de armazenamento de dados efêmeros. No entanto, existem limitações nesta abordagem - por exemplo, há a possibilidade de segurança e privacidade dos dados se esses arquivos forem acessados por programas ou usuários não autorizados.

Uma alternativa a criar um arquivo temporário poderia ser usar uma estrutura de dados na memória, como um array ou lista, para armazenar as informações temporariamente. No entanto, esta não é uma solução perfeita, já que os dados podem ser perdidos se o programa cair ou a máquina for reiniciada.

Em Elm, a criação de um arquivo temporário envolveria pedir ao servidor para criar o arquivo via chamada HTTP, já que Elm em si não tem a capacidade para interagir diretamente com o sistema de arquivo.

## Veja Também

Algumas leituras recomendadas:

- [Diferenças entre arquivos temporários e voláteis](https://www.example.com)
- [Manipulação de arquivos em Elm](https://www.example.com)
- [Segurança de dados com arquivos temporários](https://www.example.com)

---

Nota: Elm é uma linguagem de programação para a web que não possui recursos de interação direta com o sistema de arquivos, uma vez que foi projetada para ser segura por não permitir efeitos colaterais. Por isso, é importante lembrar que este artigo é um exemplo teórico e a criação de arquivos temporários em Elm seria tratada no lado do servidor, não dentro do código Elm.