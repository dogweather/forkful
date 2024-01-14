---
title:    "Haskell: Escrevendo para o erro padrão"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão em Haskell?

Escrever para o erro padrão em Haskell é uma prática importante para garantir que seu código esteja funcionando corretamente e para facilitar a depuração de erros. Ao direcionar mensagens de erro para o erro padrão, você pode tornar seu código mais legível e compreensível, economizando tempo e esforço na detecção e correção de possíveis problemas.

## Como fazer

Em Haskell, o processo de escrever para o erro padrão é bastante simples. Basta utilizar a função "hPutStr" do módulo System.IO, passando como parâmetro a mensagem de erro que você deseja exibir. Veja o exemplo abaixo:

```Haskell
import System.IO

main = do
  hPutStr stderr "Erro: variável não declarada"
```

Ao executar esse código, a mensagem "Erro: variável não declarada" será enviada para o erro padrão. É importante notar que é necessário importar o módulo System.IO e especificar o "stderr" como o primeiro argumento da função.

## Profundidade

Ao usar a função "hPutStr" em conjunto com a função "when", você pode controlar quando as mensagens de erro serão exibidas. A função "when" tem como parâmetros uma condição booleana e uma ação a ser executada caso essa condição seja verdadeira. Isso pode ser útil em situações em que você deseja exibir uma mensagem de erro apenas em certas circunstâncias.

Mas como fazer para exibir informações mais detalhadas sobre o erro? Você pode usar a função "error" do módulo Prelude, que permite exibir uma mensagem de erro com um rastreamento de pilha. Veja o exemplo abaixo:

```Haskell
import Control.Monad (when)
import System.IO

main = do
  let x = 10
  when (x > 20) (error "Valor de x deve ser menor que 20")
```

Ao executar esse código, a mensagem de erro será exibida juntamente com o rastreamento de pilha, que mostrará o local onde o erro ocorreu.

## Veja também

- [Documentação do módulo System.IO](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-IO.html)
- [Documentação do módulo Prelude](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html)
- [Tutorial básico de Haskell](https://www.haskell.org/tutorial/index.html)