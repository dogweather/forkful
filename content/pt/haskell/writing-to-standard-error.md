---
title:                "Escrevendo no erro padrão"
html_title:           "Haskell: Escrevendo no erro padrão"
simple_title:         "Escrevendo no erro padrão"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que

Você pode se perguntar por que alguém se interessaria em escrever para o erro padrão ao invés de simplesmente imprimir mensagens na tela. Bem, a resposta é simples: escrever para o erro padrão é útil quando você quer informar o usuário sobre um problema ou erro específico que ocorreu durante a execução do seu código.

## Como fazer

Escrever para o erro padrão em Haskell é bastante simples. Você pode usar a função `hPrint` do módulo `System.IO` para imprimir uma mensagem de erro em formato de string. Por exemplo:

```Haskell
import System.IO

main = do
    hPrint stderr "Ops, algo deu errado!"
```

Isso irá imprimir a mensagem "Ops, algo deu errado!" no erro padrão (standard error). Note que estamos usando `stderr` em vez de `stdout`, que seria o padrão para a função `print`.

Outra opção é usar a função `hPutStrLn`, que também pertence ao módulo `System.IO`, para imprimir uma mensagem sem a necessidade de passar uma string como argumento. Por exemplo:

```Haskell
import System.IO

main = do
    hPutStrLn stderr "Escrevendo para o erro padrão."
```

Isso irá imprimir a mensagem "Escrevendo para o erro padrão." no erro padrão.

## Mergulho profundo

Agora que você já sabe como escrever para o erro padrão em Haskell, é importante entender algumas coisas sobre esse processo. A primeira é que ao escrever para o erro padrão, sua mensagem será impressa na tela junto com a saída do seu programa. Isso é útil quando você quer dar ao usuário informações sobre possíveis erros que ocorreram.

Além disso, é importante mencionar que você também pode utilizar a função `hPutStr` para imprimir uma string sem uma quebra de linha no final. Por exemplo:

```Haskell
import System.IO

main = do
    hPutStr stderr "Esse é um texto "
    hPutStrLn stderr "que será impresso na mesma linha."
```

Isso irá imprimir "Esse é um texto que será impresso na mesma linha." sem uma quebra de linha entre os textos.

## Veja também

Se você quiser se aprofundar ainda mais no assunto, você pode dar uma olhada nos links abaixo:

- [Documentação do módulo System.IO](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html)
- [Tutorial de programação em Haskell](https://wiki.haskell.org/Introduction)