---
date: 2024-01-26 03:50:18.606575-07:00
description: "Como fazer: O Lua n\xE3o vem com um depurador embutido, mas voc\xEA\
  \ pode usar depuradores externos, como o ZeroBrane Studio. Aqui est\xE1 um gostinho\
  \ de como voc\xEA\u2026"
lastmod: '2024-03-13T22:44:46.713415-06:00'
model: gpt-4-0125-preview
summary: "O Lua n\xE3o vem com um depurador embutido, mas voc\xEA pode usar depuradores\
  \ externos, como o ZeroBrane Studio."
title: Usando um depurador
weight: 35
---

## Como fazer:
O Lua não vem com um depurador embutido, mas você pode usar depuradores externos, como o ZeroBrane Studio. Aqui está um gostinho de como você trabalharia com ele:

```Lua
-- Este é um script Lua simples com um erro intencional
local function add(a, b)
    local result = a + b -- Ops, vamos fingir que esquecemos de definir 'b'
    return result
end

print(add(10))
```

Quando você executar isso em um depurador, ele interromperá a execução onde as coisas derem errado. Você verá algo assim:

```
lua: exemplo.lua:3: tentativa de realizar aritmética em um valor nil (local 'b')
rastreamento de pilha:
	exemplo.lua:3: na função 'add'
	exemplo.lua:7: no bloco principal
	[C]: em ?
```

Você pode definir pontos de interrupção, avançar pelo seu código e espiar os valores das variáveis para rastrear o bug sem perder a sanidade.

## Mergulho Profundo
Infelizmente, a simplicidade do Lua não se estende à depuração. Mas não se preocupe, a comunidade Lua está aqui para ajudar. Ferramentas como o ZeroBrane Studio, LuaDec e outras oferecem capacidades de depuração. Historicamente, os depuradores existem desde pouco depois que os primeiros programas apresentaram problemas, dando aos desenvolvedores os meios para corrigir seu código sem mexer às cegas.

Com o Lua, você geralmente depende de depuradores externos ou os incorpora ao seu ambiente de desenvolvimento. O ZeroBrane Studio, por exemplo, é um IDE que integra totalmente um depurador Lua. Ele permite avançar pelo código, definir pontos de interrupção e observar variáveis. Do lado da implementação, os depuradores normalmente usam ganchos para inserir pontos de interrupção e outras facilidades de depuração.

Alternativas? Com certeza. Os bons e velhos comandos `print`, carinhosamente conhecidos como "depuração printf", às vezes podem fazer o truque sem ferramentas sofisticadas.

## Veja Também
Para continuar sua jornada de depuração, confira:

- ZeroBrane Studio: https://studio.zerobrane.com/
- Wiki dos usuários de Lua sobre Depuração de Código Lua: http://lua-users.org/wiki/DebuggingLuaCode
- A referência da biblioteca `debug` no manual do Lua: https://www.lua.org/manual/5.4/manual.html#6.10
