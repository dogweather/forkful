---
title:                "Usando um shell interativo (REPL)"
date:                  2024-01-26T04:16:05.712166-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando um shell interativo (REPL)"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
REPL significa Read-Eval-Print Loop (Laço de Ler-Avaliar-Imprimir), um ambiente interativo onde você pode testar código rapidamente. Programadores o utilizam para experimentar, depurar e aprender as peculiaridades de uma linguagem.

## Como Fazer:
Para entrar no REPL do Lua, basta digitar `lua` no seu terminal. Aqui está um exemplo de sessão:

```Lua
> x = 10
> print(x * 2)
20
> t = {'apple', 'banana', 'cherry'}
> table.insert(t, 'date')
> for i, fruit in ipairs(t) do print(i, fruit) end
1	apple
2	banana
3	cherry
4	date
>
```
Na sessão, declaramos uma variável, realizamos aritmética básica, manipulamos uma tabela e percorremos seus itens.

## Aprofundando
A natureza leve do Lua faz com que seu REPL seja ideal para prototipagem. Ele existe desde o início do Lua, no começo dos anos 1990, inspirado por shells interativos anteriores para linguagens como Lisp. Alternativas em outras linguagens incluem `irb` para Ruby e `python` para Python, cada uma com seu próprio conjunto de recursos. O REPL do Lua é minimalista; portanto, pode faltar recursos avançados encontrados em outros, como ferramentas complexas de depuração. Para uma experiência mais robusta, ferramentas como ZeroBrane Studio ou LuaRocks do LuaDist oferecem mais do que o básico do REPL.

## Veja Também
- [Manual de Referência do Lua 5.4 - O Interpretador Lua Standalone](https://www.lua.org/manual/5.4/manual.html#6)
- [ZeroBrane Studio](https://studio.zerobrane.com/)
- [LuaRocks](https://luarocks.org/)
