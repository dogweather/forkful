---
title:    "Arduino: Extraindo subcadeias"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Por que extrair subcadeias?

Extrair subcadeias é uma função importante de programação que pode ser útil em diversas situações. Ao extrair uma subcadeia de uma string, você está basicamente isolando uma parte específica da string original que pode ser utilizada para realizar alguma tarefa específica. Isso pode ser muito útil, por exemplo, na análise de dados ou na manipulação de textos.

## Como fazer

Para extrair subcadeias em um programa Arduino, você pode utilizar a função `substring()`. Esta função requer dois parâmetros: o índice inicial da subcadeia (onde a extração deve começar) e o tamanho da subcadeia (quantos caracteres devem ser extraídos). Confira o exemplo abaixo:

```Arduino
String nome = "Arduino";
String sub = nome.substring(0, 3);
```

Neste exemplo, a subcadeia extraída será "Ard", começando no índice 0 e com tamanho 3. O resultado será armazenado na variável `sub` e pode ser utilizado em outras partes do programa.

## Profundando um pouco mais

Além da função `substring()`, existem outras formas de extrair subcadeias em um programa Arduino. Por exemplo, é possível utilizar a função `indexOf()` para encontrar a posição de um determinado caractere ou sequência de caracteres em uma string e, a partir disso, utilizar a função `substring()` para extrair a subcadeia desejada.

Também é importante lembrar que, ao trabalhar com strings, é preciso considerar o tamanho da memória disponível no Arduino. Strings muito grandes podem ocupar muito espaço e resultar em erros de memória. Por isso, é recomendado utilizar funções como `strncpy()` ou `strlcpy()` para copiar apenas uma parte da string original, evitando desperdício de memória.

## Veja também

- [Função `substring()` da String do Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- [Manipulação de strings em linguagem C](https://www.programiz.com/c-programming/c-strings)