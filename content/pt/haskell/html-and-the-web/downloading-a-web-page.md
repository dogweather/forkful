---
date: 2024-01-20 17:44:02.204631-07:00
description: 'Como Fazer: Instale a biblioteca `http-conduit` com `cabal install http-conduit`
  e use o seguinte exemplo.'
lastmod: '2024-03-13T22:44:46.624151-06:00'
model: gpt-4-1106-preview
summary: Instale a biblioteca `http-conduit` com `cabal install http-conduit` e use
  o seguinte exemplo.
title: "Baixando uma p\xE1gina da web"
weight: 42
---

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
