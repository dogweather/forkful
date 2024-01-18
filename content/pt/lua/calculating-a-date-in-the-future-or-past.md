---
title:                "Calculando uma data no futuro ou passado"
html_title:           "Lua: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Calcular uma data no futuro ou no passado significa determinar uma data específica a partir de uma data de referência. Os programadores frequentemente realizam esse tipo de cálculo para automatizar processos ou para obter informações históricas.

## Como Fazer:

```
-- Calcular a data atual
print(os.date("%d/%m/%Y"))

-- Calcular a data de 5 dias no futuro
print(os.date("%d/%m/%Y", os.time() + 5*24*60*60))

-- Calcular a data de 2 semanas no passado
print(os.date("%d/%m/%Y", os.time() - 2*7*24*60*60))

-- Saída:
-- 30/10/2020
-- 04/11/2020
-- 16/10/2020
```

## Mergulho Profundo:

Existem diversas maneiras de calcular uma data no futuro ou no passado, e isso pode depender do contexto histórico ou do objetivo final do cálculo. Entre as alternativas, destacam-se o uso do módulo `os.date`, que permite formatar datas, e o uso de bibliotecas especializadas como a `luatz`, que oferecem funções mais precisas e diversificadas para manipulação de datas.

Além disso, é importante atentar-se à questão de fuso horário, pois o cálculo pode ser afetado pelo local onde o código está sendo executado. Também é importante considerar a formatação da data de acordo com a localização do usuário, utilizando a função `os.setlocale` para definir o idioma e o formato desejado.

## Veja Também:

- [Documentação Oficial do Lua](https://www.lua.org/docs.html)
- [Módulo os.date](https://www.lua.org/manual/5.3/manual.html#6.9)
- [Biblioteca luatz](https://github.com/luazen/lua-5.3.4)