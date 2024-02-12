---
title:                "Refatoração"
aliases:
- /pt/cpp/refactoring/
date:                  2024-01-26T01:17:06.673610-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refatoração"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/refactoring.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Refatoração é o processo de alterar a estrutura interna de um programa de computador sem alterar seu comportamento externo. Os programadores fazem isso para limpar seu código, tornando-o mais fácil de entender, manter e estender.

## Como fazer:

Imagine que você tem uma função que está fazendo um pouco demais, como este método pesado que inicializa um objeto e também realiza registros:

```C++
#include <iostream>

class Widget {
public:
    void init(bool verbose) {
        // Lógica de inicialização
        // ...

        // Registro detalhado
        if (verbose) {
            std::cout << "Widget inicializado!" << std::endl;
        }
    }
};

// Uso:
Widget w;
w.init(true);
```

Saída:
```
Widget inicializado!
```

Refatorar isso em métodos mais limpos e focados poderia parecer assim:

```C++
#include <iostream>

class Widget {
public:
    void init() {
        // Apenas lógica de inicialização
        // ...
    }

    void logInitialization() const {
        std::cout << "Widget inicializado!" << std::endl;
    }
};

// Uso:
Widget w;
w.init();
w.logInitialization();
```

Essa mudança não alterou o que o programa faz, mas torna a classe `Widget` mais modular e seu uso mais claro.

## Aprofundamento

O conceito de refatoração, como o conhecemos hoje, tem suas raízes nas comunidades de programação Smalltalk da década de 1980 e foi fortemente popularizado pelo livro de Martin Fowler "Refatoração: Aperfeiçoando o Design do Código Existente" de 1999. Hoje, a refatoração é uma parte essencial do desenvolvimento de software moderno, integrada a várias metodologias de desenvolvimento, como Agile e TDD (Desenvolvimento Orientado a Testes).

Quando falamos sobre alternativas à refatoração, entramos no terreno da reescrita ou do redesenho. A refatoração é estratégica e incremental, enquanto uma reescrita pode descartar o código existente em favor de uma nova solução. O redesenho, por outro lado, pode envolver mudanças mais significativas, incluindo alterar a funcionalidade, o que é um não-objetivo para a refatoração pura.

Os detalhes de implementação da refatoração podem se tornar bastante granulares. Existem muitos "cheiros de código" que podem motivar uma refatoração, como métodos longos, classes grandes ou código duplicado. Ferramentas automatizadas existem e podem auxiliar na refatoração, como o "Clang-Tidy" para C++, que pode identificar problemas e até aplicar algumas correções.

Além disso, a refatoração requer um conjunto sólido de testes para garantir que a funcionalidade permaneça inalterada. Sem testes, você está essencialmente voando às cegas e arriscando regressões.

## Veja também

Para um entendimento mais profundo da refatoração e para ver mais exemplos, você pode querer conferir:

- O texto clássico de Martin Fowler "Refatoração: Aperfeiçoando o Design do Código Existente" para ideias e estratégias fundamentais.
- A documentação do `Clang-Tidy` em https://clang.llvm.org/extra/clang-tidy/ para suporte automatizado à refatoração em C++.
- "Trabalhando Efetivamente com Código Legado" por Michael Feathers, que fornece técnicas para refatorar com segurança no contexto de bases de códigos existentes menos que perfeitas.
