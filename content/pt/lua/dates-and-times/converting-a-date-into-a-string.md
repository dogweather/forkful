---
date: 2024-01-20 17:37:13.179955-07:00
description: "Converter uma data em uma string significa transformar um objeto de\
  \ data, com sua estrutura pr\xF3pria, em texto simples. Fazemos isso para facilitar\
  \ a\u2026"
lastmod: 2024-02-19 22:05:05.772894
model: gpt-4-1106-preview
summary: "Converter uma data em uma string significa transformar um objeto de data,\
  \ com sua estrutura pr\xF3pria, em texto simples. Fazemos isso para facilitar a\u2026"
title: Convertendo uma data em uma string
---

{{< edit_this_page >}}

## O Que & Porquê?
Converter uma data em uma string significa transformar um objeto de data, com sua estrutura própria, em texto simples. Fazemos isso para facilitar a leitura humana, salvar em arquivos de texto ou para integração com outras partes do código que exigem texto.

## Como Fazer:
```
Lua
os.setlocale('pt_BR')  -- Definir a localidade para português do Brasil
local agora = os.date("*t")  -- Obter a data e hora atual

-- Formatar data e hora como string: Dia/Mês/Ano Horas:Minutos:Segundos
local dataString = string.format("%02d/%02d/%04d %02d:%02d:%02d", agora.day, agora.month, agora.year, agora.hour, agora.min, agora.sec)
print(dataString)  -- Mostrar a data formatada

-- Exemplo de saída: 23/02/2023 15:45:10
```

## Mergulho Profundo
Historicamente, cada sistema operacional e linguagem de programação tem seu próprio jeito de manusear datas e strings, o que pode ser um problema na hora de compartilhar dados entre sistemas diferentes. Em Lua, utilizamos a biblioteca padrão `os` para pegar datas e a função `string.format` para converter para o formato desejado. Existem outras bibliotecas, como o `os.date` para formatos mais complexos. Cuidado com fusos horários e particularidades locais ao converter datas em strings para uso internacional.

## Ver Também
- Documentação oficial do Lua sobre a biblioteca `os`: https://www.lua.org/manual/5.4/manual.html#6.9
- Lua Users Wiki sobre datas e horas: http://lua-users.org/wiki/DateTime
- Tutorial completo de Lua: https://www.tutorialspoint.com/lua/index.htm
