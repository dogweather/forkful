---
title:                "Refatoração"
date:                  2024-01-26T01:18:28.332037-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refatoração"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/refactoring.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Refatoração é o processo de ajustar seu código sem alterar seu comportamento externo. É tudo sobre limpar e organizar sua abordagem para tornar o código mais fácil de ler, manter e estender. Isso também pode ajudar a eliminar bugs e melhorar o desempenho.

## Como fazer:
Vamos supor que você tenha um bloco de código Haskell que está se repetindo mais do que sua música favorita. Aqui está uma rápida olhada em como você pode refatorar isso usando funções.

Antes da refatoração:

```haskell
printInvoice :: String -> Float -> String -> IO ()
printInvoice cliente total item = do
  putStrLn $ "Cliente: " ++ cliente
  putStrLn $ "Total: " ++ show total
  putStrLn $ "Item: " ++ item
```

Depois de um pouco de refatoração:

```haskell
printDetail :: String -> String -> IO ()
printDetail etiqueta valor = putStrLn $ etiqueta ++ ": " ++ valor

printInvoice :: String -> Float -> String -> IO ()
printInvoice cliente total item = do
  printDetail "Cliente" cliente
  printDetail "Total" (show total)
  printDetail "Item" item

-- Saída de exemplo:
-- Cliente: Alice
-- Total: $42.00
-- Item: Guia de Programação Haskell
```

Como você pode ver, ao extrair o padrão comum para uma função `printDetail` separada, evitamos repetições e tornamos `printInvoice` mais claro e fácil de gerenciar.

## Mergulho Profundo
Quando Haskell surgiu no final dos anos 80, ficou claro que o paradigma funcional poderia trazer um novo ar para as práticas de codificação. Avançando no tempo, e a refatoração em Haskell é particularmente elegante graças às funções serem cidadãos de primeira classe e ao seu sistema de tipo estático forte. Você refatora sem temer que quebraria seu aplicativo, já que o compilador te protege.

Alternativas para refatoração manual podem incluir o uso de ferramentas automatizadas, embora a natureza funcional e a segurança de tipo do Haskell possam às vezes tornar isso menos prevalente em comparação com outras linguagens. Em termos de implementação, é importante aproveitar os recursos do Haskell, como funções de ordem superior, pureza e imutabilidade para tornar a refatoração mais suave.

Refatorações como "Extrair Função", recém demonstrada, são comuns, mas você também pode fazer "Incorporar Função", "Renomear Variável" e "Alterar Assinatura de Função" com confiança, graças ao sistema de tipos. A poderosa inferência de tipo do Haskell às vezes pode capturar erros que passariam despercebidos em outras linguagens.

## Veja Também
Para um mergulho profundo em refatoração no Haskell, consulte o livro "Refactoring: Improving the Design of Existing Code" de Martin Fowler, onde os conceitos são universalmente aplicáveis. Confira a ferramenta hlint para dicas automatizadas sobre como melhorar seu código Haskell. Além disso, visite a wiki do Haskell (https://wiki.haskell.org/Refactoring) para insights da comunidade e leituras adicionais.
