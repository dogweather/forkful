---
title:                "C: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar uma string em C?

Capitalizar uma string significa deixar todas as letras maiúsculas. Embora isso possa parecer uma tarefa trivial, pode ser útil em muitos aspectos da programação. Por exemplo, pode ser necessário capitalizar nomes próprios em um sistema de cadastro ou garantir que uma entrada de usuário seja padronizada antes de ser comparada com outra string. Neste artigo, vamos explorar como capitalizar uma string em C e entender sua importância.

## Como fazer:

Para capitalizar uma string em C, precisamos seguir alguns passos simples:

1. Primeiro, definimos a string que desejamos capitalizar.

```
char str[20] = "ola mundo";
```

2. Criamos um loop que percorre cada caractere da string.

```
for (int i = 0; str[i] != '\0'; i++) {
     // codigo vai aqui 
}
```

3. Dentro do loop, verificamos se o caractere atual é uma letra minúscula. Se sim, subtraímos 32 do seu valor ASCII para transformá-lo em letra maiúscula.

```
if (str[i] >= 'a' && str[i] <= 'z') {
     str[i] -= 32;
}
```

4. Por fim, imprimimos a string resultante.

```
printf("%s", str);
```

Ao executar o código acima, a saída será "OLA MUNDO". Veja o código completo abaixo:

```
// código completo para capitalizar uma string
#include<stdio.h>

int main() {
    char str[20] = "ola mundo";
    
    // loop para percorrer os caracteres da string
    for (int i = 0; str[i] != '\0'; i++) {
        // verificando se o caractere atual é minúsculo
        if (str[i] >= 'a' && str[i] <= 'z') {
            // transformando em maiúsculo subtraindo 32 do seu valor ASCII
            str[i] -= 32;
        }
    }
    
    // imprimindo a string resultante
    printf("%s", str);
    
    return 0;
}
```

## Aprofundando:

Agora que sabemos como capitalizar uma string em C, é importante entender como o código funciona e sua complexidade. Quando verificamos se o caractere é uma letra minúscula, estamos fazendo uma comparação utilizando o valor ASCII da tabela de caracteres. Apenas letras minúsculas terão valores entre 97 e 122, por isso é importante essa condição em nosso código.

Além disso, o loop percorre cada caractere da string, o que pode ser considerado uma operação custosa em termos de desempenho. Em casos onde a string é muito grande, isso pode afetar a performance do programa. Por isso, é importante otimizar o código sempre que possível.

Em alguns casos, também pode ser necessário lidar com caracteres especiais ou acentuações em uma string, o que pode adicionar uma complexidade ao processo de capitalização.

## Veja também:

- [Como verificar se uma string é palíndromo em C](https://www.geeksforgeeks.org/c-program-check-given-string-palindrome/)
- [Como inverter uma string em C](https://www.programiz.com/c-programming/examples/reverse-string)
- [Tabela ASCII](https://pt.wikipedia.org/wiki/ASCII)