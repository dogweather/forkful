---
title:    "Haskell: Escrevendo para o erro padrão"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o stderr?

Às vezes, como programadores Haskell, queremos ter controle total sobre as saídas do nosso código. Escrever para o stderr permite que exibamos mensagens de erro ou avisos específicos durante a execução do programa.

## Como fazer

Para escrever para o stderr em Haskell, podemos usar a função `hPutStrLn` do módulo `System.IO`. Esta função recebe como parâmetro uma alça (handle) para o stderr e uma string que será exibida. Veja um exemplo:

```Haskell
import System.IO

main = do
  h <- openFile "/dev/stderr" WriteMode
  hPutStrLn h "Mensagem de erro"
  hClose h
```

Aqui, usamos a função `openFile` para abrir uma alça para o stderr, informando que queremos escrever nele (WriteMode). Em seguida, chamamos a função `hPutStrLn` para exibir a mensagem desejada e, por fim, fechamos a alça com `hClose`.

## Mergulho profundo

Uma coisa interessante sobre escrever para o stderr é que podemos usar a mesma lógica para escrever em qualquer outra fonte, como um arquivo ou um socket. Basta abrir uma alça para o desejado e usar a função `hPutStrLn`.

Também é importante lembrar que, se estivermos escrevendo uma aplicação que será executada em um terminal, o stderr será a única forma de exibir informações para o usuário. Portanto, é importante fazer bom uso dessa ferramenta para fornecer uma experiência de usuário mais amigável.

## Veja também

- [Documentação oficial do módulo System.IO](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html)
- [Tutorial sobre IO em Haskell](https://wiki.haskell.org/IO_inside)
- [Exemplo de código usando a função hPutStrLn](https://rosettacode.org/wiki/Window_creation/X11#Haskell)