---
title:                "Título em português: Capitalizando uma string."
html_title:           "Swift: Título em português: Capitalizando uma string."
simple_title:         "Título em português: Capitalizando uma string."
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# O que & Porquê?

Capitalizar uma string é simplesmente tornar sua primeira letra maiúscula. Isso pode ser útil quando se trabalha com nomes de pessoas ou títulos de livros, onde a capitalização correta é importante para apresentação visual. Programadores também fazem isso para seguir as convenções de codificação e tornar seus códigos mais legíveis para outros.

# Como fazer:

Veja três maneiras de capitalizar uma string no Swift:

```Swift
// Usando o método capitalized:
let meuNome = "michael"
let nomeCapitalizado = meuNome.capitalized
// Saída: "Michael"

// Usando o operador de concatenação:
let meuTitulo = "a arte da programação"
let tituloCapitalizado = meuTitulo.prefix(1).capitalized + meuTitulo.dropFirst()
// Saída: "A arte da programação"

// Usando uma extensão de string:
extension String {
    func capitalizado() -> String {
        return prefix(1).capitalized + dropFirst()
    }
}

let minhaCidade = "rio de janeiro"
let cidadeCapitalizada = minhaCidade.capitalizado()
// Saída: "Rio de janeiro"
```

# Profundidade:

A prática de capitalizar strings é comum em muitas linguagens de programação, incluindo Swift. Ela remonta ao tempo das máquinas de escrever, onde letras maiúsculas e minúsculas eram representadas por tipos diferentes de teclas. Também existem alternativas para capitalizar strings, como o método lowercased para torná-las totalmente minúsculas e o método uppercased para torná-las totalmente maiúsculas. Além disso, existem opções para manipular casos específicos, como usar acentos ou ligaduras em palavras capitais em diferentes línguas.

# Veja também:

Para saber mais sobre as possibilidades de formatação de strings no Swift, consulte a documentação oficial da Apple em: [https://developer.apple.com/documentation/swift/string/](https://developer.apple.com/documentation/swift/string/)