---
title:    "Haskell: Extraindo Substrings"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que

Se você é um programador Haskell, provavelmente já se deparou com situações em que precisava extrair uma parte específica de uma string. Talvez você queira obter apenas o nome de um arquivo sem sua extensão, ou talvez queira remover determinados caracteres especiais de uma string. Independentemente do motivo, saber como extrair substrings é uma habilidade importante para qualquer programador.

## Como fazer

Extrair substrings em Haskell é muito simples. Existem algumas funções úteis que nos permitem fazer isso de forma eficaz. Por exemplo, a função `take` nos permite pegar os primeiros n caracteres de uma string:

```Haskell
take 4 "Hello World" 
-- Output: "Hell"
```

Da mesma forma, a função `drop` nos permite eliminar os primeiros n caracteres de uma string:

```Haskell
drop 6 "Hello World"
-- Output: "World"
```

Podemos também usar a função `splitAt` para dividir uma string em duas partes, no índice selecionado:

```Haskell
splitAt 6 "Hello World"
-- Output: ("Hello ", "World")
```

Além disso, existem outras funções mais genéricas, como `substring`, que nos permite extrair uma sequência específica de caracteres de uma string:

```Haskell
substring 6 5 "Hello World"
-- Output: "World"
```

## Mergulho Profundo

Além das funções mencionadas acima, existem outras formas de extrair substrings em Haskell. Podemos usar expressões regulares para encontrar padrões específicos em uma string e, em seguida, extrair a parte desejada.

Outra opção é usar o pacote `text`, que oferece uma ampla gama de funções para trabalhar com strings, incluindo a extração de substrings.

## Veja também

- [Documentação do Haskell](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/text-1.2.3.0/Data-Text.html)
- [Tutorial de expressões regulares em Haskell](https://alexcd.dev/blog/making-use-of-regular-expressions-in-haskell.html)
- [Exemplos de uso do pacote 'text'](https://github.com/haskell/text/tree/master/examples)