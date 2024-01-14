---
title:    "Go: Extraindo substrings"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que usar extração de substrings em Go?

A extração de substrings é uma técnica muito útil em programação, especialmente em linguagens como Go. Ela permite que você selecione apenas partes específicas de uma string, o que pode ser útil em várias situações, como formatação de texto ou análise de dados.

## Como fazer isso em Go

A extração de substrings em Go é bastante simples. Primeiro, é preciso entender a estrutura básica para acessar uma substring em uma string. Ela segue o seguinte formato: ```string[índice inicial:índice final]```.

Vamos ver um exemplo prático. Digamos que temos a seguinte string: ```"Olá, mundo!"```. Se quisermos extrair apenas a palavra "mundo" dessa string, podemos usar o seguinte código em Go:

```Go
str := "Olá, mundo!"
sub := str[5:10]
fmt.Println(sub)
```

Esse código vai imprimir "mundo" na tela, já que o índice inicial 5 corresponde à letra "m" e o índice final 10 corresponde à letra "o". Isso mostra como é simples extrair substrings em Go.

Agora, se quisermos pegar apenas a palavra "Olá" da string, podemos usar o mesmo código, mudando apenas os índices:

```Go
str := "Olá, mundo!"
sub := str[0:3]
fmt.Println(sub)
```

Esse código vai imprimir "Olá" na tela. Note que o índice final é 3, mas a letra "á" também é incluída na substring. Isso acontece porque o índice final não é considerado, apenas o índice inicial é incluído. Então, para extrair uma substring até o final da string, basta deixar o índice final em branco:

```Go
str := "Olá, mundo!"
sub := str[5:]
fmt.Println(sub)
```

Esse código vai imprimir "mundo!" na tela, já que não há o índice final especificado e, portanto, a substring vai até o final da string. Vale lembrar que os índices em Go começam em 0, então é preciso ter isso em mente ao selecionar as partes da string que se deseja extrair.

## Mergulho profundo

Agora que você já sabe como extrair substrings em Go, é importante entender algumas coisas adicionais sobre esse processo. Primeiro, é possível usar números negativos como índices. Isso significa que, se você quiser extrair a substring "Olá" da string "Olá, mundo!", também pode fazer dessa forma:

```Go
str := "Olá, mundo!"
sub := str[:3]
fmt.Println(sub)
```

O resultado será o mesmo: "Olá". Isso acontece porque o índice inicial -3 em Go refere-se a 3 espaços antes do último caractere da string.

Além disso, vale lembrar que a extração de substrings não modifica a string original. Ela apenas retorna uma nova string com a parte selecionada. Então, se você quiser alterar a string original, é preciso atribuir a ela o valor da nova substring.

## Veja também

- [Documentação oficial do Go sobre extração de substrings](https://golang.org/ref/spec#Slice_expressions)
- [Artigo sobre extração de substrings em Go (em inglês)](https://www.callicoder.com/extract-substring-from-string-golang/)  
- [Tutorial completo sobre manipulação de strings em Go (em português)](https://aprendago.com/2019/04/23/manipulando-strings-em-go/)