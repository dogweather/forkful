---
title:    "Swift: Excluindo caracteres que coincidem com um padrão"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Por que
Existe uma ferramenta muito útil em Swift para deletar caracteres que correspondem a um determinado padrão em uma string. Isso pode ajudar os programadores a simplificar o código e torná-lo mais eficiente. Neste blog post, vamos explorar este recurso e aprender a utilizá-lo em seus projetos em Swift.

## Como fazer
Para deletar caracteres que correspondem a um padrão em Swift, utilizamos o método `replacingOccurrences()` em uma string. Vamos supor que temos uma string chamada `frase` e queremos deletar todas as letras "a" presentes nela. Podemos fazer isso da seguinte forma:

```Swift
let frase = "Eu amo Swift"
let novaFrase = frase.replacingOccurrences(of: "a", with: "")
print(novaFrase)
```

A saída deste código será "Eu mo Swift", já que todas as letras "a" foram deletadas da string original. Podemos utilizar esse método com qualquer padrão de caracteres que desejarmos, tornando-o muito versátil.

## Deep Dive
O método `replacingOccurrences()` aceita diversos parâmetros, o que nos permite personalizar ainda mais a sua utilização. Além do padrão que queremos deletar, podemos especificar a partir de qual índice da string queremos começar a procurar, quantas vezes queremos que o padrão seja substituído e também qual será o novo padrão a ser inserido. Veja um exemplo:

```Swift
let frase = "Eu amo Swift"
let novaFrase = frase.replacingOccurrences(of: "a", with: "e", options: .caseInsensitive, range: nil)
print(novaFrase)
```

Neste caso, além de substituir as letras "a" por "e", também especificamos que essa substituição deve ser feita sem levar em conta se a letra é maiúscula ou minúscula. A saída deste código será "Eu emo Swift". Há diversas opções disponíveis para essa função, tornando-a muito poderosa.

## See Also
- Documentação oficial da Apple sobre o método `replacingOccurrences()`: https://developer.apple.com/documentation/foundation/nsstring/1416866-replacingoccurrences
- Tutorial em vídeo sobre como usar esse método: https://www.youtube.com/watch?v=zTxVWdUNlAU

Espero que este blog post tenha sido útil para você aprender mais sobre como deletar caracteres que correspondem a um padrão em Swift. Coloque em prática e veja como pode facilitar sua vida de programador. Até a próxima!