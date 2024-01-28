---
title:                "Usando um depurador"
date:                  2024-01-26T04:10:45.849699-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando um depurador"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/using-a-debugger.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Usar um depurador significa utilizar ferramentas especializadas para testar e inspecionar seu código à medida que ele é executado. Isso é importante porque permite ver o que está acontecendo por baixo dos panos, encontrar bugs e compreender melhor o comportamento do seu código.

## Como fazer:
Para usar o depurador no Xcode (o IDE para Swift), você pode definir pontos de interrupção, inspecionar variáveis e observar expressões. Veja um exemplo:

```Swift
func findFactorial(of number: Int) -> Int {
    if number == 0 {
        return 1
    }
    return number * findFactorial(of: number - 1)
}

let result = findFactorial(of: 5)
print(result)
```

Defina um ponto de interrupção clicando à esquerda de um número de linha no Xcode e execute o programa. Quando ele atingir o ponto de interrupção, o Xcode pausará a execução. Agora você pode:

1. Verificar os valores das variáveis.
2. Avançar sobre (executar a próxima linha) ou avançar para dentro (entrar em uma função) usando os controles do depurador.
3. Adicionar expressões à 'lista de observação' para monitorar alterações em variáveis ou constantes específicas.

Aqui está o que você pode ver na área de depuração:

```
(lldb) po number
5
(lldb) po result
120
```

## Aprofundamento:
Depuradores fazem parte da paisagem da programação desde a década de 1940, evoluindo de sistemas simples de ponto de interrupção para experiências complexas e guiadas por UI. Outras opções além do depurador integrado ao Xcode incluem ferramentas de terceiros como o LLDB (Low Level Debugger), que o Xcode utiliza por baixo dos panos. Algumas pessoas até depuram com declarações de `print()` (carinhosamente conhecidas como "depuração das cavernas"), mas isso é menos eficiente para projetos grandes ou bugs complexos. Ao usar um depurador, você está manipulando o controle de execução, a introspecção em tempo de execução e a manipulação de dados. Uma compreensão profunda desses princípios contribui muito para uma depuração eficiente.

## Veja Também:
- [Guia de Depuração do Xcode da Apple](https://developer.apple.com/documentation/xcode/debugging-your-app)
- [Guia Rápido do LLDB](https://lldb.llvm.org/use/tutorial.html)
- [Tutorial de Depuração Swift do Ray Wenderlich](https://www.raywenderlich.com/966538-arc-and-memory-management-in-swift)
