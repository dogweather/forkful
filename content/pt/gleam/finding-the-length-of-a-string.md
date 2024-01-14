---
title:    "Gleam: Encontrando o comprimento de uma string"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que

Então, você decidiu aprender Gleam? Ótimo! Com certeza, você deve estar se perguntando por que alguém se daria ao trabalho de descobrir o comprimento de uma string. Bem, a resposta é simples: saber o comprimento de uma string é um aspecto fundamental de qualquer linguagem de programação. Além disso, esse conhecimento pode ser aplicado em uma variedade de cenários, como validação de entrada de usuário, formatação de texto e muito mais.

## Como fazer

Agora que você sabe por que é importante encontrar o comprimento de uma string, é hora de descobrir como fazer isso em Gleam. Felizmente, a linguagem tem uma função conveniente chamada `gleam_stdlib.String.len` que retorna o comprimento de uma string dada.

Vejamos um exemplo:

```
Gleam import avm_stdlib

let string = "Olá, Mundo!"
let length = gleam_stdlib.String.len(string)

gleam_stdlib.Stdout.print(length)

// Saída: 12
```

Como você pode ver, primeiro importamos a biblioteca padrão do Gleam usando a palavra-chave `import`. Em seguida, definimos uma variável `string` contendo a nossa string de exemplo. Em seguida, usamos a função `gleam_stdlib.String.len` para obter seu comprimento e armazená-lo na variável `length`. Finalmente, usamos a função `gleam_stdlib.Stdout.print` para imprimir o resultado.

## Deep Dive

Agora, vamos dar uma olhada mais aprofundada nesta função. Para começar, é importante notar que a função `gleam_stdlib.String.len` é pura, o que significa que ela não tem efeitos colaterais. Isso é importante porque ajuda a manter seu código mais previsível e evita problemas comuns, como mutação não intencional de variáveis.

Outra coisa a notar é que a função `len` é extremamente eficiente. Ao contrário de algumas linguagens de programação, como JavaScript, que precisam percorrer uma string inteira para obter seu comprimento, a função `len` no Gleam retorna o valor diretamente, sem precisar iterar pela string.

Além disso, a função `len` é autodocumentada, o que significa que você pode obter informações detalhadas sobre ela simplesmente acessando sua documentação embutida usando ferramentas como o `gleam doc` ou IDEs com suporte para Gleam.

## Veja também

- Documentação Gleam para `gleam_stdlib.String`: https://gleam.run/documentation/stdlib/string#len
- Tutorial de Gleam para iniciantes: https://gleam.run/getting-started/your-first-project
- Comunidade Gleam no Discord: https://discord.gg/QU7wR9wQZR