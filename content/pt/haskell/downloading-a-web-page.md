---
title:                "Baixando uma página da web"
html_title:           "Bash: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

---

# Haskell: Baixando uma página web

---

## O que é e por quê?

Baixar uma página da web significa solicitar e receber o conteúdo HTML de um website. Programadores fazem isso para análise de dados, teste de sites, rastreamento de páginas da web, entre outros.

---

## Como fazer:

Primeiro, instale a biblioteca `http-conduit`. Para isso, use o seguinte comando no terminal:

```Haskell
cabal install http-conduit
```

Agora, use a seguinte amostra de código para baixar uma página da web:

```Haskell
import Network.HTTP.Conduit (simpleHttp)

main = do
    putStrLn "Insira a URL:"
    url <- getLine
    sourceCode <- simpleHttp url
    putStrLn (show sourceCode)
```

Ao executar este código, a resposta será o código-fonte HTML da página solicitada.

---

## Mergulho Profundo

### Contexto Histórico

O `http-conduit` é uma evolução dos primeiros pacotes Haskell `HTTP`. Ele foi criado para melhorar a conveniência e a segurança.

### Alternativas

Outros pacotes do Haskell para baixar páginas da web incluem `http-client`, `Wreq` e `req`. Cada um desses pacotes tem seus próprios pontos fortes.

### Detalhes de Implementação

O `simpleHttp` da `http-conduit` faz um pedido HTTP GET para a URL fornecida e retorna a resposta. Ele gerencia automaticamente todos os detalhes como redirecionamentos e compactação gzip.

---

## Veja também

Para um maior aprofundamento sobre `http-conduit`, visite [a documentação oficial](https://www.stackage.org/haddock/lts-5.1/http-conduit-2.1.8/Network-HTTP-Conduit.html).

Para problemas comuns e suas soluções ao usar `http-conduit`, confira [esta discussão](https://stackoverflow.com/questions/15660356/how-to-use-http-conduit-in-haskell).

Além disso, para uma visão geral das solicitações HTTP em Haskell, [este tutorial](https://wiki.haskell.org/How_to_write_a_Haskell_web_service_%28from_scratch%29) pode ser útil.