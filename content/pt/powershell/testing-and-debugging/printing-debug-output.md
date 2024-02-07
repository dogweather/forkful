---
title:                "Exibindo saídas de depuração"
date:                  2024-01-20T17:53:24.489868-07:00
model:                 gpt-4-1106-preview
simple_title:         "Exibindo saídas de depuração"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/printing-debug-output.md"
---

{{< edit_this_page >}}

## O Que é e Por Que?

Depurar é como procurar uma agulha no palheiro: você precisa ver o que seu código está realmente fazendo. Programadores imprimem mensagens de depuração para entender o funcionamento interno ou encontrar o ponto onde as coisas vão mal.

## Como Fazer:

Imprimir no PowerShell é simples. Use `Write-Host` para enviar texto para a tela, ou `Write-Debug` para mensagens de debug que podem ser ativadas ou desativadas. Aqui estão alguns exemplos:

```PowerShell
# Imprime simples no console
Write-Host "Estou aqui!"

# Saída de depuração (não aparecerá a menos que o debug esteja ativado)
Write-Debug "Está é uma mensagem de depuração."

# Ativando mensagens de depuração
$DebugPreference = 'Continue'
Write-Debug "Agora você me vê."
```

Ao rodar com `$DebugPreference` desabilitado, você só verá "Estou aqui!". Quando habilitado, verá ambas mensagens.

## Aprofundamento:

Historicamente, imprimir saídas para depuração é uma das técnicas mais antigas e diretas. Alternativas no PowerShell incluem `Write-Verbose` e `Write-Information` para mais granularidade no controle de saída. Implementar saída de depuração é mais sobre o que você quer ver do que como você faz isso. Pensar cuidadosamente sobre *o que* imprimir é tão importante quanto saber *como* imprimir.

## Veja Também:

- Mais sobre preferências do PowerShell: [about_Preference_Variables](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_preference_variables)
