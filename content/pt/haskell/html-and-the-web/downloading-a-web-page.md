---
aliases:
- /pt/haskell/downloading-a-web-page/
date: 2024-01-20 17:44:02.204631-07:00
description: "Fazer download de uma p\xE1gina web \xE9 o processo de puxar os dados\
  \ brutos de um site para ver ou processar localmente. Programadores fazem isso para\
  \ coletar\u2026"
lastmod: 2024-02-18 23:08:58.197984
model: gpt-4-1106-preview
summary: "Fazer download de uma p\xE1gina web \xE9 o processo de puxar os dados brutos\
  \ de um site para ver ou processar localmente. Programadores fazem isso para coletar\u2026"
title: "Baixando uma p\xE1gina da web"
---

{{< edit_this_page >}}

## O Quê & Porquê?
Fazer download de uma página web é o processo de puxar os dados brutos de um site para ver ou processar localmente. Programadores fazem isso para coletar dados, testar aplicativos ou automatizar tarefas.

## Como Fazer:
Instale a biblioteca `http-conduit` com `cabal install http-conduit` e use o seguinte exemplo:

```Haskell
import Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpLBS "http://example.com"
    let statusCode = getResponseStatusCode response
    if statusCode == 200
        then putStrLn $ "Foi baixado com sucesso! Código de status: " ++ show statusCode
        else print $ "Algo deu errado! Código de status: " ++ show statusCode
    print $ getResponseBody response
```

Rodando isso, você verá algo do tipo:

```
"Foi baixado com sucesso! Código de status: 200"
"<!doctype html>..."
```

## Aprofundamento
O download de páginas web não é novidade, mas Haskell o torna elegante com sua tipagem forte e efeitos colaterais controlados. A bilbioteca `http-conduit` é só uma das ferramentas; alternativas incluem `wreq` e `http-client`. Detalhe importante do `http-conduit` é sua habilidade de lidar com solicitações streaming e processamento de resposta incrementais.

## Veja Também
- [http-conduit on Hackage](https://hackage.haskell.org/package/http-conduit)
- [http-client on Hackage](https://hackage.haskell.org/package/http-client)
- [wreq on Hackage](https://hackage.haskell.org/package/wreq)
- [Tutorial de Haskell](http://learnyouahaskell.com/chapters)
