---
title:    "Swift: Capitalizando uma string"
keywords: ["Swift"]
---

{{< edit_this_page >}}

#Por Que

Capitalize é uma função útil em programação Swift que permite que você converta a primeira letra de uma string para maiúscula. Isso é útil em várias situações, como exibir nomes ou títulos de forma mais elegante e legível.

##Como Fazer

Para usar a função capitalize, é necessário primeiro criar uma variável ou constante com uma string.

``Swift
let nome = "maria" 
var nomeMaiusculo = nome.capitalized 
print(nomeMaiusculo) //output: Maria
``

No exemplo acima, criamos a variável "nome" com a string "maria" e usamos a função capitalize para converter a primeira letra em maiúscula e atribuí-la à variável "nomeMaiusculo". Ao imprimir o valor de "nomeMaiusculo", obtemos "Maria" como resultado.

Também podemos usar a função capitalize em uma string diretamente, sem a necessidade de atribuí-la a uma variável.

``Swift
let titulo = "meu livro favorito" 
print(titulo.capitalized) //output: Meu livro favorito
``

##Mergulho Profundo

A função capitalize é útil não apenas para converter a primeira letra, mas também pode ser usada para converter a primeira letra de cada palavra em uma string para maiúscula.

``Swift
let frase = "aprendendo a programar em swift" 
print(frase.capitalized) //output: Aprendendo A Programar Em Swift 
``

Além disso, a função capitalize tem um parâmetro opcional chamado "locale", que especifica a região que será usada para realizar a conversão. Por padrão, o locale é o mesmo usado pelo dispositivo, mas você também pode especificar um locale específico para ter uma conversão mais precisa de acordo com as regras gramaticais daquela região.

``Swift
let nome = "joão" 
var nomeMaiusculo = nome.capitalized(with: Locale(identifier: "pt_BR")) 
print(nomeMaiusculo) //output: João
``

#Veja Também

- Documentação Oficial da Função Capitalize em Swift: https://developer.apple.com/documentation/swift/string/3127512-capitalized
- Guia Completo de Strings em Swift: https://www.raywenderlich.com/2270126-swift-string-cheat-sheet
- Tutorial em Português sobre Strings em Swift: https://medium.com/@andreluisgomes/manipulando-strings-em-swift-parte-i-4de17148764d