---
title:                "Capitalizando uma string"
aliases:
- pt/elm/capitalizing-a-string.md
date:                  2024-02-03T19:05:17.789713-07:00
model:                 gpt-4-0125-preview
simple_title:         "Capitalizando uma string"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?

Capitalizar uma string envolve transformar o caractere inicial de uma dada string para maiúsculo enquanto mantém o resto em minúsculo, frequentemente para fins de formatação padronizada ou legibilidade. Programadores frequentemente realizam essa tarefa para garantir que os dados sejam apresentados de forma consistente, especialmente em interfaces de usuário ou ao processar e exibir entradas de usuários.

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
