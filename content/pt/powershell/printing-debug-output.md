---
title:                "Imprimindo saída de depuração"
html_title:           "PowerShell: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/printing-debug-output.md"
---

{{< edit_this_page >}}

# Depuração de Código no PowerShell

## O que e por que?

Depurar código é um processo importante na programação, onde você imprime mensagens de saída durante a execução de um programa. Isso permite que você identifique erros e bugs em seu código e conserte-os mais facilmente. Os programadores fazem isso para facilitar a resolução de problemas e melhorar a qualidade do seu código.

## Como fazer:

Aqui estão alguns exemplos de código que utilizam a impressão de saída de depuração no PowerShell:

```PowerShell
# Imprime uma mensagem simples no console
Write-Host "Iniciando o programa de depuração..."

# Imprime o valor de uma variável
$numero = 10
Write-Host "O valor atual da variável é: $numero"

# Condições de impressão
if ($numero -gt 10) {
  Write-Host "O número é maior que 10"
}
else {
  Write-Host "O número é menor ou igual a 10"
}
```

Output:

```
Iniciando o programa de depuração...
O valor atual da variável é: 10
O número é menor ou igual a 10
```

Você também pode usar o comando `Write-Debug` para imprimir mensagens de depuração no PowerShell, mas lembre-se de que elas só serão impressas se você ativar o modo de depuração. Para fazer isso, adicione o parâmetro `-Debug` no final do comando ao chamar o seu script.

## Desvendando:

### Contexto histórico:

A depuração de código sempre foi uma parte importante do desenvolvimento de software. Antes do surgimento de ferramentas e técnicas de depuração modernas, os programadores costumavam utilizar a impressão de saída como uma maneira rápida e eficaz de encontrar erros em seus programas. O PowerShell oferece uma variedade de recursos de depuração, tornando o processo ainda mais fácil e eficiente.

### Alternativas:

Existem outras formas de depurar código, como o uso de depuradores de linha de comando, que permitem que os programadores parem a execução do código em pontos específicos e inspecionem variáveis e dados em tempo real. No entanto, a impressão de saída continua sendo uma técnica comumente utilizada em conjunto com outras ferramentas de depuração.

### Detalhes de implementação:

Para usar a função de impressão de saída no PowerShell, você pode simplesmente usar o comando `Write-Host` seguido de uma mensagem ou variável que deseja imprimir. Você também pode usar o comando `Write-Debug` para imprimir mensagens de depuração apenas quando necessário. Lembre-se de ativar o modo de depuração com o parâmetro `-Debug` para que essas mensagens sejam exibidas.

## Veja também:

- [Artigo da Microsoft sobre como depurar scripts do PowerShell](https://docs.microsoft.com/pt-br/powershell/scripting/debugging/debugging-scripts?view=powershell-7)
- [Vídeo tutorial da Microsoft sobre depuração de código no PowerShell](https://www.youtube.com/watch?v=5IY0hxESO5Q)
- [Fórum de discussão sobre depuração de código no PowerShell](https://stackoverflow.com/questions/tagged/powershell+debugging)