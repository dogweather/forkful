---
title:                "Javascript: Extraindo subcadeias"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extrair substrings é útil na programação Javascript?

Extrair substrings é uma técnica útil na programação Javascript que permite aos desenvolvedores segmentar uma string em partes menores. Isso pode ser útil em uma variedade de situações, como fazer ajustes em dados, validar entradas do usuário ou manipular informações em um formato específico.

Essa habilidade é particularmente útil em projetos nos quais os desenvolvedores precisam trabalhar com grandes quantidades de dados, ou quando precisam fazer mudanças precisas em uma string sem alterar o conteúdo original. Agora, vamos dar uma olhada em como extrair substrings em Javascript.

## Como extrair substrings em Javascript

Extrair substrings em Javascript é simples e pode ser feito usando o método `.substring()`. O formato básico deste método é o seguinte:

```Javascript
var string = "Hello, world!";
var substring = string.substring(startIndex, endIndex);
console.log(substring);
```

Neste exemplo, a string "Hello, world!" é atribuída à variável `string`. Em seguida, usamos o método `.substring()` para extrair uma parte da string, passando o índice inicial e final como parâmetros. O valor retornado será "Hello,".

Podemos usar o método `.substring()` para extrair mais do que apenas uma parte da string. Por exemplo, se passarmos apenas o parâmetro de índice inicial, ele extrairá o restante da string a partir desse ponto. Vamos dar uma olhada em outro exemplo:

```Javascript
var string = "Hello, world!";
var substring = string.substring(7);
console.log(substring);
```

Neste exemplo, a substring "world!" será extraída da string original.

## Aprofundando-se em extrair substrings

O método `.substring()` permite que os desenvolvedores trabalhem com strings de forma mais precisa. Ele também tem a vantagem de ser um método imutável, o que significa que ele não altera a string original. Isso pode ser útil em situações em que precisamos usar a string original, mas também precisamos de uma versão modificada dela.

Além disso, o método `.substring()` é semelhante ao método `.slice()`, mas com algumas diferenças. Por exemplo, o método `.slice()` suporta índices negativos, o que significa que podemos extrair substrings começando do final da string.

## Veja também

- [Documentação do método `.substring()` no MDN](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [Como extrair substrings em Python](https://www.treinaweb.com.br/blog/extraia-uma-substring-de-uma-string-em-python/)
- [Tutorial de Javascript para iniciantes](https://www.devmedia.com.br/aprendendo-javascript-para-iniciantes-verdadeiro-primeiro-codigo/23986)