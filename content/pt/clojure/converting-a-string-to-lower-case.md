---
title:    "Clojure: Convertendo uma string para letras minúsculas"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# Por que

Converter uma string para minúsculas é uma tarefa comum em programação, pois muitas vezes precisamos comparar ou manipular strings de diferentes maneiras. Felizmente, em Clojure, este processo é bastante simples e eficiente.

## Como Fazer

Em Clojure, existe uma função nativa chamada `lower-case` que transforma toda a string em minúsculas. Veja o exemplo abaixo:

```Clojure
(lower-case "STRING DE EXEMPLO") ;; retorna "string de exemplo"
```

Você também pode usar a função `map` em conjunto com `lower-case` para transformar uma lista de strings em minúsculas:

```Clojure
(map lower-case ["STRING 1" "STRING 2" "STRING 3"]) ;; retorna ("string 1" "string 2" "string 3")
```

## Mergulho Profundo

É importante notar que, ao converter uma string para minúsculas, a função `lower-case` seguirá as regras de capitalização Unicode. Isso significa que, dependendo do idioma, certos caracteres podem ter uma forma maiúscula e minúscula diferente.

Além disso, essa função também pode levar em consideração as configurações locais (locale) do sistema operacional, alterando a forma como certos caracteres são transformados.

Se você quiser ter mais controle sobre o processo de conversão de minúsculas, pode usar a biblioteca `clojure.string` e suas funções `lower-case` ou `lower-case-locales`. Esta biblioteca também oferece funções para converter strings para maiúsculas e para capitalizar apenas a primeira letra de cada palavra (título).

# Veja Também

- Página oficial da documentação de strings em Clojure: https://clojure.org/reference/strings
- Guia de utilização da biblioteca `clojure.string`: https://clojure.org/guides/strings