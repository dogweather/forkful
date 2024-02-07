---
title:                "Usando expressões regulares"
date:                  2024-02-03T19:17:29.132573-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando expressões regulares"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?

Expressões regulares em programação permitem a correspondência e manipulação de strings baseadas em padrões específicos. Programadores as usam para tarefas como validação, pesquisa e manipulação de texto devido à sua versatilidade e eficiência no tratamento de operações complexas com strings.

## Como:

Lua não suporta expressões regulares nativamente da mesma forma que linguagens como Perl ou Python. Em vez disso, oferece capacidades de correspondência de padrões que cobrem muitos casos de uso comuns de expressões regulares. No entanto, para um suporte completo a expressões regulares, pode-se usar uma biblioteca de terceiros, como `lrexlib`.

### Correspondência de Padrões Básica em Lua:

Lua fornece um sistema de correspondência de padrões poderoso que você pode usar para substituições e pesquisas simples:

```lua
-- Pesquisa simples
local str = "Olá, Mundo!"
if string.find(str, "Mundo") then
  print("Correspondência encontrada!")
end
-- Saída: Correspondência encontrada!

-- Substituição simples
local s = string.gsub("Lua é ótimo!", "ótimo", "incrível")
print(s)
-- Saída: Lua é incrível!
```

### Capturando Substrings:

Você pode capturar partes da string que correspondem aos padrões:

```lua
local data = "Hoje é 17/05/2023."
local d, m, a = string.match(data, "(%d+)/(%d+)/(%d+)")
print("Dia:", d, "Mês:", m, "Ano:", a)
-- Saída: Dia: 17 Mês: 05 Ano: 2023
```

### Usando `lrexlib` para Expressões Regulares:

Para usar expressões regulares de fato, você pode instalar e usar `lrexlib`. Supondo que você tenha instalado (`luarocks install lrexlib-pcre`), você pode fazer correspondências de padrões mais complexas:

```lua
local rex = require 'rex_pcre'

local texto = "A chuva na Espanha permanece principalmente na planície."
local regex = "\\bS\\w+"
local contagem, erro = rex.gsub(texto, regex, function(w)
  return w:upper()
end)
if erro then
  print("Erro:", erro)
else
  print("Texto modificado:", texto)
  print("Substituições feitas:", contagem)
end
-- Exemplo de saída: Texto modificado: A CHUVA na ESPANHA permanece PRINCIPALMENTE na planície.
-- Substituições feitas: 3
```

Os exemplos acima ilustram o uso básico dentro do próprio sistema de correspondência de padrões de Lua e como aproveitar o poder das expressões regulares via `lrexlib`. Seja você realizando simples manipulações de string ou requerendo a versatilidade completa das expressões regulares, Lua, juntamente com poderosas bibliotecas, pode atender às suas necessidades.
