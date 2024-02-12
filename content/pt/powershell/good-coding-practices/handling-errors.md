---
title:                "Tratamento de erros"
aliases: - /pt/powershell/handling-errors.md
date:                  2024-01-26T00:56:40.804817-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tratamento de erros"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/handling-errors.md"
---

{{< edit_this_page >}}

## O que e Por quê?
Tratar erros no PowerShell significa prever os percalços e gerenciá-los de maneira suave. Programadores fazem isso para prevenir falhas e fornecer aos usuários um feedback útil.

## Como fazer:
```PowerShell
# Tentativa básica com Try-Catch para tratar exceções
try {
    # Código que pode desencadear um erro
    $result = 1 / 0
} catch {
    # O que fazer se ocorrer um erro
    Write-Host "Ops, ocorreu um erro: $_"
}

# Emitindo uma mensagem de erro personalizada
try {
    Get-Item "arquivoinexistente.txt" -ErrorAction Stop
} catch {
    Write-Host "O arquivo não pôde ser encontrado."
}

# Usando a variável $Error para inspecionar o último erro
```
## Aprofundando
O PowerShell evoluiu muito desde sua criação como Monad. O tratamento de erros tornou-se mais robusto ao longo do tempo, oferecendo funcionalidades semelhantes a outras linguagens de programação. A sintaxe `try-catch-finally` é um desses exemplos de compartilhamento entre linguagens como C#. Antes disso, os scripters dependiam muito da verificação de condições e do uso da variável automática `$Error`.

O PowerShell também tem dois principais tipos de erros: termináveis e não termináveis. Erros termináveis interrompem o script, a menos que sejam capturados em um bloco `try-catch`, enquanto os não termináveis não o farão, a menos que você especifique `-ErrorAction Stop`. Essa distinção é crucial, pois confere controle refinado sobre o tratamento de erros, decidindo se um erro realmente justifica a parada total do script ou pode simplesmente ser registrado e ignorado.

O tratamento de erros do PowerShell permite também um bloco `finally`, que é executado não importa o quê - se ocorrer um erro ou não. É ótimo para tarefas de limpeza.

Quando você está profundamente envolvido na criação de scripts, também pode tratar tipos específicos de exceções, o que lhe dá ainda mais controle.

Alternativamente, há o parâmetro antigo `-ErrorVariable` para capturar erros sem lançar uma exceção. E a variável `$?` diz se a última operação foi bem-sucedida. São ferramentas úteis, embora um pouco menos limpas do que um sólido `try-catch`.

## Veja também
- [about_Try_Catch_Finally](https://docs.microsoft.com/pt-br/powershell/module/microsoft.powershell.core/about/about_try_catch_finally?view=powershell-7.2)
