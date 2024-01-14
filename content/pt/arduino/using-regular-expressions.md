---
title:    "Arduino: Usando expressões regulares"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar Expressões Regulares em Programação Arduino?

Em programação, muitas vezes precisamos procurar por padrões específicos em uma string de caracteres. Isso pode ser desde um endereço de email até um número de telefone. Usar expressões regulares em Arduino pode nos ajudar a encontrar e manipular esses padrões de forma eficiente e precisa.

## Como usar Expressões Regulares em Programação Arduino?

Para usar expressões regulares em Arduino, é necessário incluir a biblioteca "Regex.h". Em seguida, podemos usar a função "Regex.Match" para procurar um padrão específico em uma string. Veja um exemplo abaixo:

```Arduino
#include <Regex.h>

String frase = "Meu endereço de email é exemplo@email.com";

Regex padrao("([A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,4})");
// O padrão acima procura por um endereço de email válido

if (padrao.Match(frase)) {
  Serial.println("Endereço de email válido encontrado!");
} else {
  Serial.println("Nenhum endereço de email válido encontrado!");
}

// Saída:
// Endereço de email válido encontrado!
```

Outra função útil é o "Regex.Replace", que permite substituir um padrão por outro. Veja um exemplo abaixo:

```Arduino
#include <Regex.h>

String frase = "Bem-vindo ao meu website, onde você pode encontrar muitas informações.";

Regex padrao("website");
// O padrão acima procura pela palavra "website"

String novaFrase = padrao.Replace(frase, "blog");
// Agora "blog" substituiu "website" na string

Serial.println(novaFrase);
// Saída:
// Bem-vindo ao meu blog, onde você pode encontrar muitas informações.
```

## Mergulhando mais fundo em Expressões Regulares

Para entender melhor como as expressões regulares funcionam, é importante aprender sobre seus diferentes componentes, como metacaracteres, quantificadores e grupos de captura. Além disso, é útil praticar com diferentes exemplos e padrões para ganhar familiaridade com a sintaxe.

## Veja Também

- [Documentação Oficial da Biblioteca Regex.h](https://www.arduino.cc/reference/en/libraries/regex/)
- [Tutorial de Expressões Regulares para Arduino](https://create.arduino.cc/projecthub/Arnov_Sharma/regex-regular-expressions-tutorials-a24ddc)
- [Guia Completo de Expressões Regulares](https://www.regular-expressions.info/tutorial.html)