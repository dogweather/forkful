---
date: 2024-01-26 03:40:31.902542-07:00
description: "Como fazer: Aqui est\xE1 como expulsar essas aspas para longe em Lua."
lastmod: '2024-03-13T22:44:46.697658-06:00'
model: gpt-4-0125-preview
summary: "Aqui est\xE1 como expulsar essas aspas para longe em Lua."
title: Removendo aspas de uma string
weight: 9
---

## Como fazer:
Aqui está como expulsar essas aspas para longe em Lua:

```lua
local function remove_quotes(str)
  return (str:gsub("^%p(.*)%p$", "%1"))
end

print(remove_quotes('"Olá, Mundo!"'))     -- Olá, Mundo!
print(remove_quotes("'Adeus, Aspas!'"))  -- Adeus, Aspas!
```

E voilà! As aspas desapareceram como meias em uma secadora.

## Aprofundamento
As pessoas têm removido aspas de strings desde que as linguagens conseguiam manipular texto, o que é praticamente desde sempre. Em Lua, a função `gsub` faz o trabalho pesado, utilizando padrões como um bisturi para excisar as aspas. Alternativas? Claro, você poderia usar regex em linguagens que suportam isso, ou escrever seu próprio loop que passa por cada caractere (bocejo, mas ei, é o seu tempo).

O sistema de correspondência de padrões de Lua lhe dá a potência de uma experiência tipo regex-lite sem importar uma biblioteca inteira. O acento circunflexo (`^`) e o sinal de dólar (`$`) correspondem ao início e ao fim da string, respectivamente; `%p` corresponde a qualquer caractere de pontuação. Depois de se livrar da pontuação no início e no fim, capturamos tudo o mais com `(.*),` e substituímos toda a correspondência com esse grupo de captura usando `" %1"`.

Lembre-se, a correspondência de padrões de Lua não é tão potente quanto os motores de regex completos - por exemplo, ela não pode contar ou retroceder. Essa simplicidade é tanto uma bênção quanto uma maldição, dependendo de quais aspas você está tentando domar e onde elas estão escondidas.

## Veja Também
Mergulhe mais fundo na correspondência de padrões de Lua com o livro PiL (Programming in Lua): http://www.lua.org/pil/20.2.html

Para pura elegância, veja como outras linguagens fazem isso para comparação, começando com o `str.strip` do Python: https://docs.python.org/3/library/stdtypes.html#str.strip
