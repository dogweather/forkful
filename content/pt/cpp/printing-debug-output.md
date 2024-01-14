---
title:                "C++: Imprimindo saída de depuração"
programming_language: "C++"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

##Por que?

Uma ferramenta muito útil para os programadores é a impressão de saída de depuração, também conhecida como "print debugging". Isso significa imprimir informações específicas do código durante o processo de execução, permitindo que os desenvolvedores entendam melhor o que está acontecendo em seus programas. Se você é novo na programação ou quer melhorar suas habilidades, a impressão de saída de depuração pode ser uma excelente técnica para adicionar ao seu arsenal de solução de problemas.

##Como Fazer

Aqui estão alguns exemplos de como imprimir saída de depuração em C++:

```C++
// Imprime o valor de uma variável inteira
int num = 10; 
std::cout << "O valor de num é: " << num << std::endl;

// Imprime uma mensagem de erro
std::cerr << "Erro ao executar o programa." << std::endl;

// Imprime o conteúdo de um vetor
std::vector<int> vetor = {1, 2, 3, 4, 5};
std::cout << "O vetor contém os seguintes elementos: ";
for(int i = 0; i < vetor.size(); i++){
  std::cout << vetor[i] << " ";
}
std::cout << std::endl;
```

A saída desses exemplos seria:

```
O valor de num é: 10
Erro ao executar o programa.
O vetor contém os seguintes elementos: 1 2 3 4 5
```

Isso pode parecer simples, mas a impressão de saída de depuração pode ser muito útil quando você está lidando com códigos mais complexos e quer entender como algumas variáveis estão mudando durante a execução do programa.

##Profundando

Embora a impressão de saída de depuração seja uma técnica simples, ela pode ser muito útil em situações específicas. Por exemplo, quando você está trabalhando com loops e quer verificar se a condição do loop está sendo realizada corretamente, você pode imprimir o valor de cada variável envolvida para garantir que não haja erros.

Além disso, a impressão de saída de depuração é uma alternativa quando você não tem uma ferramenta de depuração adequada disponível. Em vez de interromper o processo de execução em um ponto específico do seu código, você pode simplesmente imprimir as informações de depuração e, em seguida, comentá-las ou removê-las depois que o problema for resolvido.

##Veja também

Aqui estão alguns artigos relacionados que podem ser úteis para aprimorar suas habilidades de depuração em C++:

- [10 dicas para depuração de código em C++](https://www.guru99.com/c-plus-plus-debugging-example.html)
- [Depuração eficiente usando GDB](https://www.thegeekstuff.com/2010/03/debug-c-program-using-gdb/)
- [Técnicas de depuração para programadores iniciantes em C++](https://www.learncpp.com/cpp-tutorial/67-chapter-10-debugging-challenges/)