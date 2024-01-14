---
title:    "Gleam: Buscando e substituindo texto"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Por que Utilizar Substituição de Texto em Gleam?

Substituir texto é uma habilidade essencial para qualquer programador, e em Gleam, isso pode ser feito de forma eficaz e eficiente. Ao aprender como fazer substituição de texto, você poderá economizar tempo e esforço ao editar arquivos de código extensos. 

## Como Fazer Substituição de Texto em Gleam

Para fazer substituição de texto em Gleam, você pode seguir os seguintes passos:

- Passo 1: Importe o módulo `gleam/text`:

```
Gleam.Text
```

- Passo 2: Utilize a função `replace` para fazer a substituição. Por exemplo, se você quiser substituir todas as letras "a" por "e" em uma string, você pode fazer o seguinte:

```
Gleam.Text.replace("banana", "a", "e")
```

A saída seria "`benene`".

- Passo 3: Você também pode usar expressões regulares para substituir padrões específicos de texto. Por exemplo, se você quiser substituir todas as vogais em uma string por "`*`", você pode fazer o seguinte:

```
Gleam.Text.replace_regex("Hello world!", "[aeiou]", "*")
```

A saída seria "`H*ll* w*rld!`".

## Mergulho Profundo

Existem algumas coisas importantes a serem lembradas ao fazer substituição de texto em Gleam. Primeiro, é importante certificar-se de que os argumentos da função `replace` estejam na ordem correta - a string de entrada, o texto a ser substituído e o novo texto. Além disso, você também pode usar padrões de expressão regular mais complexos para substituição de texto, permitindo maior flexibilidade ao realizar essa operação.

## Veja também

- Documentação oficial sobre o módulo `gleam/text`: https://gleam.run/modules/gleam/text.html
- Exemplos práticos de substituição de texto em Gleam: https://github.com/gleam-lang/gleam/blob/master/build/bazel/generate_code.sh
- Tutorial sobre expressões regulares em Gleam: https://dev.to/yugisu/quick-intro-to-expressions-regulars-in-gleam-12o2