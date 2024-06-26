---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:17.789713-07:00
description: "Como fazer: No Elm, n\xE3o h\xE1 uma fun\xE7\xE3o integrada espec\xED\
  fica para capitalizar strings. No entanto, voc\xEA pode alcan\xE7ar isso facilmente\
  \ usando as fun\xE7\xF5es do\u2026"
lastmod: '2024-03-13T22:44:46.482184-06:00'
model: gpt-4-0125-preview
summary: "No Elm, n\xE3o h\xE1 uma fun\xE7\xE3o integrada espec\xEDfica para capitalizar\
  \ strings."
title: Capitalizando uma string
weight: 2
---

## Como fazer:
No Elm, não há uma função integrada específica para capitalizar strings. No entanto, você pode alcançar isso facilmente usando as funções do módulo `String` integrado como `toUpper`, `toLower`, `left` e `dropLeft`.

```elm
capitalize : String -> String
capitalize str =
    if String.isEmpty str then
        ""
    else
        String.toUpper (String.left 1 str) ++ String.toLower (String.dropLeft 1 str)

-- Exemplo de uso
main =
    String.toList "hello world" |> List.map capitalize |> String.join " "
    -- Saída: "Hello World"
```

Para cenários mais complexos ou se você prefere usar uma biblioteca que forneça uma maneira direta de capitalizar strings, você pode considerar um pacote de terceiros, como `elm-community/string-extra`. No entanto, conforme minha última atualização, o ecossistema do Elm incentiva lidar com tais tarefas usando funções integradas para manter a linguagem e os projetos enxutos.

```elm
import String.Extra as StringExtra

-- Caso haja uma função de `capitalize` em uma biblioteca de terceiros
capitalizeWithLibrary : String -> String
capitalizeWithLibrary str =
    StringExtra.capitalize str

-- Exemplo de uso com função hipotética de biblioteca
main =
    "this is elm" |> capitalizeWithLibrary
    -- Saída hipotética: "This is elm"
```

Sempre verifique o repositório de pacotes do Elm para as bibliotecas mais recentes e mais preferidas para manipulação de strings se você estiver procurando por funcionalidades adicionais além da biblioteca padrão.
