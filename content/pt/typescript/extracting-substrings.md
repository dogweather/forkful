---
title:    "TypeScript: Extraindo subcadeias"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extrair substrings?

Extrair substrings é uma técnica útil e comum na programação TypeScript. Isso nos permite manipular e trabalhar com strings de maneira mais eficiente e flexível. Com a extração de substrings, podemos obter partes específicas de uma string sem precisar modificar toda a string original.

## Como fazer

Para extrair substrings em TypeScript, existem duas opções: podemos usar o método `substring()` ou o operador de indexação `[ ]`.

### Método `substring()`

Podemos usar o método `substring()` para extrair uma substring de uma string existente. Ele aceita dois parâmetros: o índice inicial e o índice final da substring que desejamos extrair.

```TypeScript
let nome = "João Pedro";
let sobrenome = nome.substring(5, 11);
console.log(sobrenome); // Imprime "Pedro"
```

Neste exemplo, usamos o método `substring()` para extrair o sobrenome "Pedro" da string "João Pedro". O parâmetro `5` representa o índice inicial da substring e o parâmetro `11` representa o índice final. É importante notar que o índice final não é incluído na substring final, portanto, devemos usar o índice `11` para obter a última letra "o" do sobrenome.

### Operador de Indexação `[ ]`

Outra opção para extrair substrings em TypeScript é usar o operador de indexação `[ ]`. Com ele, podemos acessar caracteres específicos de uma string, incluindo uma sequência de caracteres. Veja o exemplo abaixo:

```TypeScript
let curso = "Programação TypeScript";
let linguagem = curso[13, 22];
console.log(linguagem); // Imprime "TypeScript"
```

Aqui, usamos o operador de indexação para extrair a string "TypeScript" da string "Programação TypeScript". O parâmetro `13` representa o índice inicial da substring e o parâmetro `22` representa o índice final. Assim como no método `substring()`, o índice final não é incluído na substring final.

## Mergulho Profundo

Agora que já conhecemos as duas maneiras de extrair substrings em TypeScript, é importante lembrar que os índices do método `substring()` e do operador de indexação `[ ]` são baseados em zero. Isso significa que o primeiro caractere da string tem o índice `0` e o último caractere tem o índice `length - 1`, onde `length` é o tamanho total da string.

Além disso, o método `substring()` também aceita apenas números positivos para os índices. Isso significa que o índice inicial deve ser menor do que o índice final. Caso contrário, o método retornará uma string vazia.

## Veja também

- [Documentação oficial da Microsoft sobre o método `substring()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [Documentação oficial da Microsoft sobre o operador de indexação `[ ]`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/slice) 
- [Artigo sobre o operador de indexação no blog da Comunidade TypeScript](https://blog.typescriptlang.org/2019/02/11/using-the-index-type-operator/)