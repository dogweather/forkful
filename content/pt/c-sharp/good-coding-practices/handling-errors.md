---
date: 2024-01-26 00:50:44.946263-07:00
description: "O tratamento de erros em C# \xE9 sobre gerenciar o inesperado\u2014\
  como trope\xE7ar nos pr\xF3prios cadar\xE7os. Programas podem trope\xE7ar em dados\
  \ ruins ou conex\xF5es\u2026"
lastmod: '2024-03-13T22:44:46.591359-06:00'
model: gpt-4-1106-preview
summary: "O tratamento de erros em C# \xE9 sobre gerenciar o inesperado\u2014como\
  \ trope\xE7ar nos pr\xF3prios cadar\xE7os. Programas podem trope\xE7ar em dados\
  \ ruins ou conex\xF5es\u2026"
title: Tratamento de erros
weight: 16
---

## O Quê e Por Quê?

O tratamento de erros em C# é sobre gerenciar o inesperado—como tropeçar nos próprios cadarços. Programas podem tropeçar em dados ruins ou conexões instáveis. Nós tratamos os erros para manter nosso software de dar de cara no chão, permitindo que ele se recupere de forma elegante.

## Como fazer:

Vamos começar com um bloco try-catch. É como colocar uma rede de segurança sob um equilibrista na corda bamba. Se escorregarem, eles não despencam—são pegos.

``` C#
using System;

class ExemploTratamentoErro {
    static void Main() {
        try {
            int[] numeros = {1, 2, 3};
            Console.WriteLine(numeros[5]);  // Ops, índice fora dos limites!
        } catch (IndexOutOfRangeException e) {
            Console.WriteLine("Erro capturado: " + e.Message);
        }
    }
}
```

Saída de exemplo quando as coisas dão errado:
```
Erro capturado: Índice estava fora dos limites do array.
```

Agora adicionamos um bloco finally—é o que acontece não importa o quê, como pagar impostos.

``` C#
try {
    // Código potencialmente problemático aqui
} catch (AlgumaExcecaoEspecífica e) {
    // Aqui se manuseia aquele erro específico
} finally {
    // Este código roda não importa o que aconteça acima
    Console.WriteLine("Isto sempre roda.");
}
```

## Mergulho Profundo

O tratamento de erros está no C# desde seu nascimento. Com o tempo, ele evoluiu. Antigamente, programadores dependiam de códigos de retorno ou flags globais para sinalizar problemas—desajeitado e propenso a erros.

C# utiliza exceções, uma abordagem mais moderna. Uma exceção é lançada quando o inesperado acontece, assim como jogar uma bandeira na jogada no futebol. O tratamento estruturado de exceções com os blocos try, catch e finally torna o gerenciamento destes momentos mais claro e limpo do que as antigas verificações de erro.

Alternativas? Claro. Há o `UnhandledExceptionEventHandler` para exceções que escapam. Ou em código assíncrono, o tratamento de erros muda um pouco com objetos `Task` que carregam as suas próprias exceções.

Detalhes de implementação—semelhantes a letras miúdas—importam. Exceções podem ser custosas, derrubando o desempenho se lançadas à toa. Por isso, as usamos para casos excepcionais, não para controle lógico do dia a dia.

## Veja Também

- [Documentação oficial sobre Exceções em C#](https://docs.microsoft.com/pt-br/dotnet/csharp/fundamentals/exceptions/exception-handling)
- [Melhores práticas no tratamento de exceções em C#](https://docs.microsoft.com/pt-br/dotnet/standard/exceptions/best-practices-for-exceptions)
