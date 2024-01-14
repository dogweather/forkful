---
title:                "TypeScript: Convertendo uma string para minúsculas."
simple_title:         "Convertendo uma string para minúsculas."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que converter uma string para minúsculas?

Há várias razões para se converter uma string para minúsculas ao trabalhar com programação em TypeScript. Uma das principais razões é padronizar os dados de entrada em um formato comum, facilitando a comparação e manipulação dos mesmos. Além disso, algumas funções e métodos específicos só funcionam corretamente com strings em minúsculas, portanto, é importante realizar essa conversão antes de utilizá-los.

## Como fazer:

```TypeScript
// Exemplo de função para converter uma string para minúsculas
function converterParaMinusculas(texto: string): string {
    return texto.toLowerCase();
}

let textoEntrada: string = "ESTE TEXTO SERÁ CONVERTIDO PARA MINÚSCULAS";
console.log(converterParaMinusculas(textoEntrada)); // saída: "este texto será convertido para minúsculas"
```

No código acima, temos uma função simples que utiliza o método `toLowerCase()` da classe `string` para converter o texto para minúsculas. Basta passar a string desejada como parâmetro e a função irá retornar a mesma string em formato de letras minúsculas.

Além disso, é importante lembrar que a conversão para minúsculas é case sensitive, ou seja, as letras maiúsculas serão convertidas para suas respectivas minúsculas, mas as minúsculas já existentes permanecerão iguais.

```TypeScript
let textoEntrada: string = "Este é um TEXTO de ExeMplO";
console.log(converterParaMinusculas(textoEntrada)); // saída: "este é um texto de exemplo"
```

## Mergulho Profundo:

Agora que já sabemos como converter uma string para minúsculas em TypeScript, podemos nos aprofundar um pouco mais e entender como isso é feito internamente.

Ao chamar o método `toLowerCase()` de uma string, o TypeScript utiliza a funcionalidade `toLocaleLowerCase()` do JavaScript. Essa funcionalidade retorna uma string com todas as letras convertidas para suas respectivas minúsculas, dependendo das regras do idioma fornecido. Por padrão, se nenhum idioma for especificado, a conversão será realizada de acordo com o idioma do ambiente em que o código está sendo executado.

Outra coisa importante a se mencionar é que o método `toLowerCase()` não altera a string original, mas sim retorna uma nova string com as alterações realizadas. Portanto, para alterar a string original, é necessário atribuir o resultado do método a uma nova variável ou sobrescrever a variável original.

## Veja também:

- [Documentação Oficial do Método `toLowerCase()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Lista de Idiomas Disponíveis para Conversão](https://unicode.org/cldr/charts/latest/supplemental/language_plural_rules.html)