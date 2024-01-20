---
title:                "Pesquisando e substituindo texto"
html_title:           "Bash: Pesquisando e substituindo texto"
simple_title:         "Pesquisando e substituindo texto"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O Que é & Por Quê?
Pesquisar e substituir texto é uma operação comum no desenvolvimento de software que permite localizar certos caracteres ou strings e alterá-los por outro(s). Programadores fazem isso para corrigir erros, modificar a funcionalidade ou simplificar o código.

## Como Fazer:
Aqui está um exemplo simples sobre como você pode procurar e substituir texto em C.

```C
#include <string.h>

void substituir(char *s, char ch1, char ch2) {
    for (int i = 0; s[i] != '\0'; ++i) {
        if (s[i] == ch1) {
            s[i] = ch2;
        }
    }
}

int main() {
    char s[] = "Ola, mundo!";
    substituir(s, 'O', 'H');
    printf("%s\n", s);
    return 0;
}
```

A saída será: `Hola, mundo!`

## Mergulhando Fundo
Historicamente, as funções de pesquisa e substituição foram implementadas pela primeira vez em editores de texto, como o `ed` e `sed` no Unix. Na programação moderna, existem várias alternativas, como o uso de funções internas de uma linguagem de programação (como `str_replace` em PHP) ou criando sua própria função personalizada, como fizemos acima. Vale a pena notar que o método acima mostrado é de padrão C simples e não eficiente para strings grandes. Métodos mais eficientes como Aho-Corasick ou Boyer–Moore-Horspool podem ser utilizados para melhor desempenho.

## Ver Também
Para aprender mais sobre os algoritmos de pesquisa e substituição de texto, cheque os seguintes links:

1. "Algoritmos de String Matching no Wikipedia":
https://pt.wikipedia.org/wiki/Categoria:Algoritmos_de_busca_de_pad%C3%A3o
2. "Tutorial de String Matching - parte de um curso gratuito de Ciência da Computação com certificado":
https://www.coursera.org/lecture/data-structures/lecture-36-string-matching-UzYS4