---
title:                "Extraindo substrings"
html_title:           "Bash: Extraindo substrings"
simple_title:         "Extraindo substrings"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

---
title: Extrair Substrings em Swift: um guia conciso
---

## O que e por que? 
Extrair substrings é um processo de dividir um string em pequenos segmentos. Programadores fazem isso para trabalhar especificamente com partes de um string e não o string inteiro.

## Como fazer:
Aqui estão algumas maneiras de extrair substrings em Swift.

```Swift
let s = "Ola, mundo!"
let index = s.index(s.startIndex, offsetBy: 4)
let substring = s[..<index] // Resulta em "Ola,"
print(substring)
```

Nesse exemplo, extraímos o substring "Ola," da string original.

Aqui está outro exemplo usando o método `prefix`:

```Swift
let s = "Ola, mundo!"
let substring = s.prefix(4) // Resulta em "Ola,"
print(substring)
```

Novamente, conseguimos extrair o substring "Ola,".

## Mergulho profundo:
Extrair substrings tem sido uma técnica utilizada desde os primeiros dias da programação para manipular e gerenciar dados de string. No Swift, a classe `String` possui uma variedade de métodos para lidar com substrings. 

Alternativamente, você pode usar o método `suffix` para obter substrings a partir do final da string. Veja:

```Swift
let s = "Ola, mundo!"
let substring = s.suffix(6) // Resulta em "mundo!"
print(substring)
```

Note que o Swift trata strings como uma coleção de valores `Character`, então a indexação e a extração são baseadas na posição do caractere, e não no valor do byte. Isso é diferente de outras linguagens como C e Python, onde a indexação de string é baseada no valor do byte.

## Veja também
Para obter mais informações sobre as capacidades de extração de substrings de Swift, você pode verificar os seguintes links:

- Documentação oficial da Apple para Substring: [https://developer.apple.com/documentation/swift/substring](https://developer.apple.com/documentation/swift/substring)
- Stack Overflow para perguntas sobre substrings em Swift: [https://stackoverflow.com/questions/tagged/swift+substring](https://stackoverflow.com/questions/tagged/swift+substring)