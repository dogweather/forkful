---
title:    "Elm: Imprimindo saída de depuração"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que

Você já teve problemas para entender por que seu código está funcionando de determinada maneira? Ou talvez você queira ver como seus dados estão sendo manipulados durante a execução do programa? Nesses casos, imprimir saídas de depuração pode ser uma ferramenta útil para ajudá-lo a compreender melhor o fluxo do seu código.

## Como Fazer

Para imprimir saídas de depuração em Elm, existem duas opções principais: a função `Debug.log` e a biblioteca [elm-debug](https://package.elm-lang.org/packages/the-sett/elm-debug/latest/). A função `Debug.log` é útil para imprimir valores de variáveis, enquanto a biblioteca elm-debug permite a criação de impressões de depuração mais detalhadas. Veja abaixo exemplos de como implementar essas opções em seu código:

```Elm
-- utilizando Debug.log
import Debug

nome = "Maria"
Debug.log "Nome:" nome

-- utilizando elm-debug
-- importe a biblioteca em seu código
import TheSett.ElmDebug

-- crie uma função para imprimir um valor
nomeCompleto primeiroNome ultimoNome =
    TheSett.ElmDebug.print "Nome Completo:" <| primeiroNome ++ " " ++ ultimoNome
```

A saída desses exemplos seria:

```
Nome: "Maria"
Nome Completo: "Maria Smith"
```

## Mergulho Profundo

Uma coisa importante a se notar sobre as impressões de depuração em Elm é que elas não são exibidas no navegador por padrão. Para ver as saídas, é necessário ativar a visualização do console no navegador. Para fazer isso, clique com o botão direito na página do seu programa Elm e selecione "Inspecionar" ou "Inspeção" (dependendo do navegador). Na janela que se abre, procure por uma guia chamada "Console". Selecione esta guia e as saídas de depuração serão exibidas aqui.

Além disso, é importante lembrar de remover todas as impressões de depuração antes de enviar seu programa para produção, pois elas podem afetar o desempenho e a segurança do seu código.

## Veja Também

- [Documentação da função Debug.log](https://package.elm-lang.org/packages/elm/core/latest/Debug#log)
- [Elm-debug no Pacote Elm](https://package.elm-lang.org/packages/the-sett/elm-debug/latest/)