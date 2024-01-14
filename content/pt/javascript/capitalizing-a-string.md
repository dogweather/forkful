---
title:                "Javascript: Maiúsculas em uma string"
simple_title:         "Maiúsculas em uma string"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar uma string?

Capitalizar uma string é uma prática comum em programação, especialmente quando estamos trabalhando com dados de entrada do usuário. A capitalização garante que nosso código lide com diferentes formas de escrita da mesma palavra ou frase de forma consistente.

## Como fazer?

Para capitalizar uma string em Javascript, podemos utilizar o método `toUpperCase()` em combinação com o método `charAt()` para alterar a primeira letra para maiúscula. Veja um exemplo de código abaixo:

```Javascript
let palavra = "javascript";

console.log(palavra.charAt(0).toUpperCase() + palavra.slice(1));
// Saída: Javascript
```

Neste exemplo, utilizamos o método `charAT()` para selecionar a primeira letra da palavra "javascript" e, em seguida, usamos o `toUpperCase()` para alterá-la para maiúscula. Depois, utilizamos o método `slice()` para selecionar todas as letras restantes da string e juntamos com a primeira letra já capitalizada.

Podemos também criar uma função em Javascript para capitalizar uma string ainda mais facilmente. Veja outro exemplo de código abaixo:

```Javascript
function capitalizarString(palavra) {
  return palavra.charAt(0).toUpperCase() + palavra.slice(1);
}

console.log(capitalizarString("programação")); 
// Saída: Programação
```

Neste exemplo, fizemos o mesmo processo descrito anteriormente, mas encapsulando o código em uma função. Assim, podemos passar qualquer palavra como parâmetro para a função e obter a versão capitalizada da mesma.

## Profundidade na capitalização de strings

Além das formas mencionadas acima, existem outras maneiras de capitalizar uma string em Javascript. Por exemplo, podemos utilizar a função `replace()` combinada com expressões regulares para encontrar e substituir letras específicas por suas versões maiúsculas. Também é possível usar bibliotecas externas para capitalizar strings em diferentes idiomas, incluindo acentuações e caracteres especiais.

No entanto, é importante lembrar que a capitalização de strings pode variar de acordo com a linguagem e o contexto em que estamos trabalhando. Por isso, é fundamental sempre ler a documentação e estar atento às melhores práticas de código.

## Veja também

- [Documentação do método `toUpperCase()` - MDN](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [Documentação do método `charAt()` - MDN](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
- [Documentação do método `slice()` - MDN](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/slice)