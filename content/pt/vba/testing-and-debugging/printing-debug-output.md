---
title:                "Imprimindo a saída de depuração"
aliases:
- /pt/vba/printing-debug-output.md
date:                  2024-02-01T21:58:17.175378-07:00
model:                 gpt-4-0125-preview
simple_title:         "Imprimindo a saída de depuração"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/vba/printing-debug-output.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Quê & Porquê?
Imprimir saídas de depuração em Visual Basic for Applications (VBA) envolve colocar estrategicamente instruções de impressão dentro do seu código para exibir valores de variáveis, fluxo de execução ou mensagens de depuração personalizadas. Esta técnica é essencial para a depuração, permitindo que os programadores entendam o comportamento do seu código em tempo de execução e identifiquem qualquer comportamento inesperado ou erros.

## Como fazer:
No VBA, a instrução `Debug.Print` é a principal ferramenta para imprimir informações de depuração na Janela Imediata no Editor Visual Basic (VBE). Para usar essa funcionalidade efetivamente, você precisa ter a Janela Imediata visível (Visualizar > Janela Imediata ou pressione `Ctrl+G` no VBE).

Aqui está um exemplo simples de uso do `Debug.Print` para exibir o valor de uma variável e uma mensagem personalizada:

```basic
Sub PrintDebugInfo()
    Dim sampleVar As Integer
    sampleVar = 42
    Debug.Print "O valor de sampleVar é: "; sampleVar
End Sub
```

Quando você executar essa sub-rotina, a Janela Imediata exibirá:
```
O valor de sampleVar é: 42
```

Você também pode usá-lo para acompanhar o fluxo de lógica condicional complexa inserindo instruções `Debug.Print` em várias ramificações do seu código:

```basic
Sub CheckValue()
    Dim valueToCheck As Integer
    valueToCheck = 9
    
    If valueToCheck > 10 Then
        Debug.Print "O valor é maior que 10."
    ElseIf valueToCheck < 10 And valueToCheck > 0 Then
        Debug.Print "O valor está entre 1 e 9."
    Else
        Debug.Print "O valor é 10 ou menos que 1."
    End If
End Sub
```

Executando `CheckValue`, produz:
```
O valor está entre 1 e 9.
```

Lembre-se, a saída do `Debug.Print` vai apenas para a Janela Imediata, o que é extremamente útil durante a fase de desenvolvimento, mas não aparece em nenhuma parte voltada para o usuário de uma aplicação.

## Aprofundamento
A Janela Imediata e o método `Debug.Print` têm raízes profundas na história do Visual Basic for Applications, refletindo a evolução das práticas de depuração ao longo do tempo. Inicialmente, a depuração era um processo mais textual e menos visual, com os desenvolvedores dependendo fortemente de instruções de impressão para entender o que seu código estava fazendo. Ao longo dos anos, à medida que os ambientes de desenvolvimento evoluíram, também evoluíram as ferramentas de depuração, introduzindo pontos de interrupção, watchdogs e ferramentas de perfilagem mais sofisticadas que proporcionam uma visão mais interativa e imediata do comportamento do código.

No entanto, `Debug.Print` e a Janela Imediata ainda são incrivelmente úteis, particularmente para sessões rápidas e práticas de depuração ou ao lidar com código que é difícil de interromper (como manipuladores de eventos). Dito isso, é importante reconhecer que depender exclusivamente de instruções de impressão para depuração na programação moderna pode ser menos eficiente em comparação com o uso de depuradores integrados com capacidades de ponto de interrupção, observação e inspeção de pilha.

Embora alternativas como frameworks de registro ou ferramentas de depuração mais avançadas ofereçam mais recursos e flexibilidade, a simplicidade e imediatismo do `Debug.Print` no VBA o tornam uma ferramenta valiosa, especialmente para programadores em transição de outras línguas que já estão acostumados com técnicas de depuração baseadas em impressão. No entanto, à medida que se tornam mais confortáveis com o VBA e o Editor Visual Basic, explorar a gama completa de ferramentas de depuração disponíveis pode levar a resoluções de problemas mais eficazes e eficientes.
