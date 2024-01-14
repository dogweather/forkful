---
title:    "TypeScript: Procurando e substituindo texto"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Por que Procurar e Substituir Texto é Importante

Procurar e substituir texto é uma tarefa comumente realizada em programação e pode economizar bastante tempo e esforço. Ao invés de ter que encontrar e modificar cada instância de um texto manualmente, é possível automatizar esse processo com uma única linha de código.

## Como Realizar Procura e Substituição de Texto em TypeScript

Para realizar procura e substituição de texto em TypeScript, podemos utilizar a função `replace` do objeto `String`. Essa função recebe dois argumentos: o primeiro é o texto que queremos procurar e o segundo é o texto que desejamos substituir. Por exemplo:

```TypeScript
let frase = "Vamos aprender TypeScript!";
frase = frase.replace("aprender", "dominar");
console.log(frase); // Saída: "Vamos dominar TypeScript!"
```

Outra opção é usar expressões regulares para procurar um padrão de texto específico e substituí-lo. Podemos fazer isso passando uma expressão regular como o primeiro argumento da função `replace` e utilizando o metacaractere `$` para indicar a parte do texto que deve ser substituída. Veja o exemplo abaixo:

```TypeScript
let texto = "O número 123 é um exemplo.";
texto = texto.replace(/\d+/g, "999");
console.log(texto); // Saída: "O número 999 é um exemplo."
```

## Aprofundando na Procura e Substituição de Texto

Além das opções básicas de procura e substituição mencionadas acima, existem outras funcionalidades e opções avançadas disponíveis. Por exemplo, é possível utilizar a função `replace` em conjunto com uma callback que determina o texto a ser substituído com base em uma lógica específica.

Também é possível utilizar flags de expressões regulares, como `i` para realizar a substituição com "case-insensitive" (ignorando maiúsculas e minúsculas) ou `g` para substituir todas as ocorrências do padrão de texto encontrado.

No geral, é importante conhecer bem as opções e recursos disponíveis para aproveitar ao máximo a funcionalidade de procura e substituição de texto em TypeScript.

## Veja Também

- [Documentação Oficial do TypeScript](https://www.typescriptlang.org/docs/)
- [Tutorial de Expressões Regulares em TypeScript](https://medium.com/javascript-in-plain-english/regular-expressions-in-typescript-a-comprehensive-tutorial-31506396bafa)
- [Referência de String em TypeScript](https://www.w3schools.com/TAgs/ref_string.asp)