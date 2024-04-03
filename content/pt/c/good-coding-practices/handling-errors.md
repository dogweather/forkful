---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:54.046209-07:00
description: "Como fazer: C n\xE3o possui suporte embutido para exce\xE7\xF5es como\
  \ algumas outras linguagens. Em vez disso, ele depende de algumas estrat\xE9gias\
  \ convencionais de\u2026"
lastmod: '2024-03-13T22:44:47.058345-06:00'
model: gpt-4-0125-preview
summary: "C n\xE3o possui suporte embutido para exce\xE7\xF5es como algumas outras\
  \ linguagens."
title: Gerenciando erros
weight: 16
---

## Como fazer:
C não possui suporte embutido para exceções como algumas outras linguagens. Em vez disso, ele depende de algumas estratégias convencionais de tratamento de erros, como retornar valores especiais de funções e configurar variáveis globais como `errno`.

**Retornando Valores Especiais**

As funções podem indicar erros retornando um valor específico que é improvável de ser um resultado válido. Aqui está um exemplo com inteiros:

```c
#include <stdio.h>

int inverse(int number, double *result) {
    if (number == 0) {
        return -1; // Caso de erro
    } else {
        *result = 1.0 / number;
        return 0; // Sucesso
    }
}

int main() {
    double result;
    if (inverse(0, &result) < 0) {
        printf("Error: Divisão por zero.\n");
    } else {
        printf("O inverso é: %f\n", result);
    }
    
    return 0;
}
```

**Saída:**
```
Error: Divisão por zero.
```

**Verificando `errno`**

Para funções de biblioteca, especialmente aquelas que interagem com o sistema ou SO (como E/S de arquivo), `errno` é definido quando ocorre um erro. Para usá-lo, inclua `errno.h` e verifique `errno` após uma falha suspeita:

```c
#include <stdio.h>
#include <errno.h>
#include <string.h>

int main() {
    FILE *file = fopen("nonexistent.txt", "r");
    if (file == NULL) {
        printf("Erro ao abrir arquivo: %s\n", strerror(errno));
    }
    
    return 0;
}
```

**Saída:**
```
Erro ao abrir arquivo: No such file or directory
```

## Aprofundamento
Historicamente, o design minimalista da linguagem de programação C excluiu um mecanismo de tratamento de exceções embutido, refletindo suas origens de programação de sistemas de baixo nível, onde o desempenho máximo e o controle próximo ao hardware são críticos. Em vez disso, o C adota uma abordagem de tratamento de erros mais manual que se encaixa em sua filosofia de dar aos programadores o máximo controle possível, mesmo à custa da conveniência.

Embora esta abordagem esteja bem alinhada com os objetivos de design do C, também pode levar a um código de verificação de erro verboso e ao potencial para verificações de erro perdidas, questões que linguagens modernas abordam com mecanismos estruturados de tratamento de exceções. Por exemplo, exceções em linguagens como Java ou C# permitem o processamento centralizado de erros, tornando o código mais limpo e a gestão de erros mais direta. No entanto, exceções introduzem seu próprio custo e complexidade, que podem não ser ideais para programação em nível de sistema onde o C brilha.

Apesar de sua crueza, esse tratamento manual de erros em C informou o design da gestão de erros em muitas outras linguagens, oferecendo um modelo onde a explicitação das condições de erro pode levar a um código mais previsível e depurável. Para sistemas críticos, onde falhas devem ser gerenciadas de forma graciosa, o paradigma de tratamento de erros do C - combinado com as melhores práticas modernas como bibliotecas e convenções de gerenciamento de erros - garante robustez e confiabilidade.
