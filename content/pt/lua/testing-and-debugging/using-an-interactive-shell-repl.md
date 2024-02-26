---
date: 2024-01-26 04:16:05.712166-07:00
description: "REPL significa Read-Eval-Print Loop (La\xE7o de Ler-Avaliar-Imprimir),\
  \ um ambiente interativo onde voc\xEA pode testar c\xF3digo rapidamente. Programadores\
  \ o\u2026"
lastmod: '2024-02-25T18:49:44.330874-07:00'
model: gpt-4-0125-preview
summary: "REPL significa Read-Eval-Print Loop (La\xE7o de Ler-Avaliar-Imprimir), um\
  \ ambiente interativo onde voc\xEA pode testar c\xF3digo rapidamente. Programadores\
  \ o\u2026"
title: Usando um shell interativo (REPL)
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
