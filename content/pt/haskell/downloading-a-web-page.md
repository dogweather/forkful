---
title:                "Baixando uma página da web"
html_title:           "Haskell: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por que 

Se você está interessado em criar aplicações web ou trabalhar com dados da internet, é importante saber como baixar uma página da web usando Haskell. Isso permite que você acesse informações úteis e crie aplicações mais interativas e dinâmicas.

## Como Fazer

Baixar uma página da web usando Haskell é um processo relativamente simples. Primeiro, precisamos importar o módulo "Network.HTTP", que nos permite fazer solicitações HTTP. Em seguida, usamos a função "simpleHttp" para fornecer o URL da página que queremos baixar. Vamos dar uma olhada em um exemplo de código:

```Haskell
import Network.HTTP

main = do
    -- Fazer a solicitação HTTP 
    response <- simpleHttp "https://example.com"
    
    -- Imprimir o corpo da resposta 
    putStrLn $ "Corpo da resposta: \n" ++ show response
```

No exemplo acima, usamos a função "putStrLn" para imprimir o corpo da resposta, que é a página que baixamos. No entanto, é importante notar que a função "simpleHttp" retorna um "IO ByteString", que é uma representação de uma sequência de bytes. Portanto, podemos converter essa representação em uma string usando a função "show".

## Profundidade

Ao baixar uma página da web usando Haskell, também podemos especificar o tipo de solicitação que queremos fazer. Por exemplo, se quisermos fazer uma solicitação POST, podemos usar a função "postSimple". Além disso, podemos definir cabeçalhos personalizados usando a função "addRequestHeader". Vamos ver um exemplo de código que usa essas funções:

```Haskell
import Network.HTTP

main = do
    -- Definir cabeçalho personalizado 
    let headers = [("Content-Type", "application/json")]
    
    -- Fazer a solicitação POST 
    response <- postSimple "https://example.com" headers "{}"
    
    -- Imprimir o corpo da resposta
    putStrLn $ "Corpo da resposta: \n" ++ show response
```

Esse exemplo nos permite fazer uma solicitação POST com um cabeçalho personalizado e um corpo de requisição JSON vazio. No entanto, existem outras funções no módulo "Network.HTTP" que nos permitem fazer solicitações mais complexas, como enviar parâmetros de formulário ou fazer solicitações HTTPS.

## Veja Também

- [Documentação do módulo "Network.HTTP" em Haskell] (https://hackage.haskell.org/package/HTTP)
- [Tutorial: Baixando páginas da web em Haskell] (https://www.schoolofhaskell.com/user/commercial/content/learning-haskell-tele-weka-part-3-simple-networking)