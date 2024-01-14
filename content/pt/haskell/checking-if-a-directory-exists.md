---
title:    "Haskell: Verificando se um diretório existe"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Por que

Ao escrever programas em Haskell, muitas vezes é necessário verificar se um diretório existe antes de realizar alguma operação com ele. Isso é importante para garantir que seu código seja mais robusto e evite possíveis erros.

## Como fazer

O processo de verificação de existência de um diretório pode ser feito usando a função `doesDirectoryExist` da biblioteca `System.Directory`. Isso retorna um valor booleano que indica se o diretório existe ou não. Veja um exemplo abaixo:

```Haskell
import System.Directory

main = do
  exists <- doesDirectoryExist "caminho/do/diretorio" 
  if exists then putStrLn "O diretório existe." 
  else putStrLn "O diretório não existe."
```

O código acima primeiro importa o módulo `System.Directory` e em seguida usa a função `doesDirectoryExist` para verificar se o diretório no caminho especificado existe. Se existir, a mensagem "O diretório existe." é exibida, caso contrário, a mensagem "O diretório não existe." é mostrada.

## Dê um mergulho

Além de verificar a existência de um diretório, também é possível obter informações mais detalhadas sobre ele usando outras funções da biblioteca `System.Directory`. Por exemplo, a função `getPermissions` retorna todas as permissões associadas ao diretório, permitindo que você verifique se ele é legível, gravável ou se possui permissões especiais.

## Veja também

- [Documentação da biblioteca System.Directory](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [Tutorial de Haskell do DataHaskell](https://www.datahaskell.org/docs/tutorials/programming-in-haskell)
- [Vídeo tutorial de Haskell do Curso em Vídeo](https://www.youtube.com/watch?v=PHSxm4XJ8Lw)