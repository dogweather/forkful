---
title:    "Elixir: Excluindo caracteres correspondentes a uma padrão"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

PORQUE: Por que você deve se engajar em excluir caracteres que correspondem a um padrão?

Excluir caracteres que correspondem a um padrão é útil ao escrever código limpo e eficiente. Isso ajuda a evitar erros de digitação em variáveis que possam causar bugs em seu programa. Além disso, pode melhorar a legibilidade do código e torná-lo mais fácil de entender para outros programadores.

COMO FAZER:

Para excluir caracteres que correspondem a um padrão em Elixir, podemos usar a função "String.replace/4". Esta função aceita quatro argumentos: a string original, o padrão a ser encontrado, a string de substituição e o número máximo de ocorrências a serem substituídas.

Vejamos um exemplo:

```
Elixir
iex> "banana" |> String.replace("na", "ra", 2) 
"barara"
```

No exemplo acima, estamos substituindo o segundo e o terceiro "na" na string "banana" por "ra". O resultado é a string "barara".

Outra função útil é "String.trim/2", que remove os caracteres especificados das bordas da string. Podemos usar isso para remover caracteres indesejados no início e no final da string.

```
Elixir
iex> "!!!olá!!!" |> String.trim("!")
"olá"
```

Esses são apenas alguns exemplos de como podemos excluir caracteres que correspondem a um padrão em Elixir. Experimente com diferentes funções e parâmetros para encontrar a melhor solução para sua situação específica.

MERGULHO PROFUNDO:

Para entender melhor como a função "String.replace/4" funciona, devemos dar uma olhada mais de perto em seu código. A função é definida dentro do módulo "String", que é parte da biblioteca padrão do Elixir.

Dentro da função, está sendo usado o operador "|>" para encadear funções juntas. Isso é uma característica fundamental do estilo de programação funcional em Elixir.

Em seguida, há uma chamada para a função "String.replace_binary/4", que usa as funções "String.to_charlist/1" e "String.from_charlist/1" para converter a string em uma lista de caracteres e vice-versa. Isso é necessário porque o Elixir trabalha com strings internamente como listas de caracteres.

Continuando, há uma chamada para a função "replace_binary/4" a partir do módulo "Binary.Replace". Esta função itera sobre a lista de caracteres e substitui as ocorrências do padrão pelo texto de substituição.

Como resultado, obtemos a string com os caracteres substituídos. Conhecer o funcionamento interno dessas funções pode ser útil ao resolver problemas complexos que exigem substituição de caracteres em strings.

VEJA TAMBÉM:

- Documentação oficial do Elixir para a função "String.replace/4": https://hexdocs.pm/elixir/String.html#replace/4
- Guia de Referência de Funções do Elixir: https://elixir-lang.org/getting-started/modules-and-functions.html
- Vídeo tutorial sobre o uso de funções de string em Elixir: https://www.youtube.com/watch?v=dX15A30TQRc

Esperamos que este artigo tenha sido útil para você aprender como excluir caracteres que correspondem a um padrão em Elixir. Continue praticando e explorando mais recursos da linguagem para melhorar suas habilidades de programação em Elixir.

VEJA TAMBÉM:
- Documentação oficial do Elixir para a função "String.replace/4": https://hexdocs.pm/elixir/String.html#replace/4
- Guia de Referência de Funções do Elixir: https://elixir-lang.org/getting-started/modules-and-functions.html
- Vídeo tutorial sobre o uso de funções de string em Elixir: https://www.youtube.com/watch?v=dX15A30TQRc