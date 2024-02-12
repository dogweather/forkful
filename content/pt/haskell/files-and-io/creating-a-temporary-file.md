---
title:                "Criando um arquivo temporário"
aliases:
- /pt/haskell/creating-a-temporary-file.md
date:                  2024-01-20T17:40:19.342292-07:00
model:                 gpt-4-1106-preview
simple_title:         "Criando um arquivo temporário"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O Que É & Porquê?
Criar um arquivo temporário é gerar um arquivo que é destinado a ser utilizado durante a execução de um programa e, geralmente, excluído após o uso. Programadores fazem isso para manter dados voláteis, como caches, ou para manipular informações sem riscos de alterar dados permanentes.

## Como Fazer:

Para criar um arquivo temporário em Haskell, você pode usar a biblioteca `temporary`. Vou te mostrar como rola:

```haskell
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)

main :: IO ()
main = withSystemTempFile "meuTemp.txt" $ \filepath handle -> do
  putStrLn $ "O arquivo temporário é: " ++ filepath
  -- Escreva no arquivo usando handle
  -- Quando o bloco terminar, o arquivo é automaticamente excluído
```

Quando você executar isso, verá algo assim:

```
O arquivo temporário é: /tmp/meuTemp.txt123456
```

Pronto! O arquivo existe enquanto seu código roda, e depois, puff, some.

## Mergulho Profundo

Antigamente, antes de termos abstrações bacanas como `withSystemTempFile`, você teria que gerenciar os arquivos temporários na mão. Isso poderia ser enrolado e trazer bugs.

Alternativas? Claro, você pode criar arquivos com nomes únicos manualmente, mas por que reinventar a roda?

Implementação... a `withSystemTempFile` cuida do ciclo de vida do arquivo temporário. Ela cria, passa o handle pro seu código, e assegura que o arquivo seja deletado após o bloco. Conveniente, né?

## Veja Também

- [Pacote temporary no Hackage](https://hackage.haskell.org/package/temporary)
- [Tutorial de IO em Haskell](http://learnyouahaskell.com/input-and-output)
- [Repositório GHC, onde você pode bisbilhotar como as coisas são feitas em Haskell](https://gitlab.haskell.org/ghc/ghc) 

Ié, isso é só um gostinho. Haskell tem muito mais sob o capô. Fuçar esses links e explorar por conta própria é uma boa pedida.
