---
title:    "C: Escrevendo testes"
keywords: ["C"]
---

{{< edit_this_page >}}

## Por que escrever testes é importante na programação

Escrever testes é uma parte essencial do processo de desenvolvimento de software. É uma forma de garantir que seu código esteja funcionando corretamente e de identificar possíveis erros antes que seu código seja disponibilizado ao público. Além disso, escrever testes ajuda a documentar seu código e a entender melhor seu funcionamento.

## Como escrever testes em C

Aqui estão algumas dicas para escrever testes em C:

```C
#include <stdio.h>

// Função que retorna a soma de dois números
int soma(int a, int b){
  return a + b;
}

int main(){
  // Testando a função soma
  int resultado = soma(5, 7);
  printf("Resultado da soma: %d\n", resultado);
  return 0;
}
```

Executando o código acima, teremos a seguinte saída:

```C
Resultado da soma: 12
```

Para escrever testes mais aprofundados, você pode utilizar bibliotecas de testes como a [check](https://libcheck.github.io/check/). Essas bibliotecas oferecem funções e macros úteis para criar testes mais robustos e organizados.

## Aprofundando nos testes

Além de simplesmente testar o resultado de uma função, é importante também testar diferentes cenários e casos de uso. Por exemplo, se sua função de soma só aceita números inteiros, é importante testar também com números decimais ou com valores muito grandes ou muito pequenos.

Outra dica é criar testes para identificar possíveis erros de lógica no código ou condições de contorno. Isso pode ajudar a prevenir erros futuros.

Além disso, é importante ter uma boa cobertura de testes, ou seja, testar o máximo de funções e cenários possíveis. Isso garante uma maior confiabilidade no seu código e facilita a identificação de problemas em caso de alterações futuras.

## Veja também

- [Documentação da biblioteca check em português](https://libcheck.github.io/check/doc/check_html/check_23.html)
- [Tutorial sobre testes em C](https://www.tutorialspoint.com/unit_testing_with_cppunit/)

Escrever testes é uma prática que pode parecer trabalhosa e desnecessária no início, mas que traz muitos benefícios no longo prazo. Não deixe de implementar essa etapa importante no desenvolvimento do seu código em C.