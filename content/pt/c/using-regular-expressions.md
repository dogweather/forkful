---
title:                "C: Usando expressões regulares"
simple_title:         "Usando expressões regulares"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Por que usar expressões regulares em programação

Expressões regulares são uma ferramenta poderosa na programação que permitem a busca por padrões em strings e textos. Com elas, é possível realizar tarefas como validação de dados, substituição de caracteres e extração de informações específicas. Além disso, sua sintaxe compacta e versatilidade tornam as expressões regulares amplamente utilizadas em diversas linguagens de programação, incluindo C. Neste artigo, vamos aprender como utilizar expressões regulares em C e explorar um pouco mais sobre sua funcionalidade.

## Como usar expressões regulares em C

Para utilizar expressões regulares em C, primeiro precisamos incluir a biblioteca correspondente com o seguinte comando:

```C
#include <regex.h>
```

Em seguida, devemos compilar a expressão regular com a função `regcomp()` que recebe como parâmetros a expressão regular e um flag que indica o comportamento desejado. Por exemplo, se quisermos fazer uma busca case sensitive, podemos utilizar o flag `REG_ICASE`.

```C
regex_t regex; // declarando a variável que irá armazenar a expressão
char *pattern = "a[0-9]+b"; // expressão regular a ser compilada
int result = regcomp(&regex, pattern, 0); // compilando a expressão regular
```

Feito isso, podemos utilizar a função `regexec()` para comparar a expressão regular com uma string específica. Essa função retorna o valor `REG_NOERROR` caso haja uma correspondência e `REG_NOMATCH` caso contrário.

```C
char *text = "a123b";
result = regexec(&regex, text, 0, NULL, 0);
if (result == REG_NOERROR) {
    printf("A string corresponde à expressão regular!\n");
} else {
    printf("A string não corresponde à expressão regular!\n");
}
```

Além disso, também podemos utilizar as funções `regwcomp()` e `regwexec()` para trabalhar com expressões regulares em formato wide char. É importante lembrar que, ao final do uso, devemos liberar a memória alocada pela expressão compilada utilizando a função `regfree()`.

## Aprofundando-se nas expressões regulares

Além dos exemplos apresentados, as expressões regulares em C possuem diversas funcionalidades e recursos que permitem a criação de regex mais complexas. Por exemplo, podemos utilizar os caracteres `?` e `*` para indicar opcionalidade e repetição, respectivamente. Também é possível utilizar agrupamentos com parênteses e até mesmo referenciar esses grupos em substituições. Para mais informações sobre os recursos disponíveis e suas sintaxes, é recomendado consultar a documentação oficial da biblioteca `regex.h`.

Além disso, sempre é válido praticar e se aprofundar nas expressões regulares, já que elas podem ser aplicadas em diversas áreas da programação e são uma ótima forma de otimizar o código.

## Veja também

- [Documentação oficial da biblioteca `regex.h` em C](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions-Functions.html#Regular-Expressions-Functions)
- [Tutorial de expressões regulares em C](https://www.regular-expressions.info/c.html)
- [Exemplos de regex em C](https://www.guru99.com/c-regex.html)