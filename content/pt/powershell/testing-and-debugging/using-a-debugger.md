---
date: 2024-01-26 04:08:48.394903-07:00
description: "Como fazer: No PowerShell, voc\xEA pode depurar scripts usando o Ambiente\
  \ de Scripting Integrado do PowerShell (ISE) ou o Visual Studio Code (VS Code) com\
  \ a\u2026"
lastmod: '2024-03-13T22:44:46.801471-06:00'
model: gpt-4-0125-preview
summary: "No PowerShell, voc\xEA pode depurar scripts usando o Ambiente de Scripting\
  \ Integrado do PowerShell (ISE) ou o Visual Studio Code (VS Code) com a extens\xE3\
  o do PowerShell."
title: Usando um depurador
weight: 35
---

## Como fazer:
No PowerShell, você pode depurar scripts usando o Ambiente de Scripting Integrado do PowerShell (ISE) ou o Visual Studio Code (VS Code) com a extensão do PowerShell. Veja como usar pontos de interrupção em ambos:

### PowerShell ISE:
```PowerShell
# Definir um ponto de interrupção em uma linha específica
Set-PSBreakpoint -Script .\MyScript.ps1 -Line 5

# Executar seu script normalmente
.\MyScript.ps1

# Quando o script atingir o ponto de interrupção, você pode inspecionar variáveis
$myVariable

# Continuar a execução
Continue
```

### Visual Studio Code:
```PowerShell
# Abra seu script PowerShell no VS Code.
# Clique à esquerda do número da linha para definir um ponto de interrupção.
# Inicie a depuração pressionando F5 ou clicando em 'Iniciar Depuração'.

# O VS Code interromperá a execução no seu ponto de interrupção.
# Use o painel de depuração para observar variáveis, inspecionar a pilha de chamadas e controlar o fluxo.
```

Depurar em ambos os ambientes permite que você entre em (F11), passe por cima (F10) e saia (Shift+F11) enquanto depura.

## Aprofundando
Historicamente, a depuração no PowerShell era um pouco complicada; era necessário muitas linhas de `Write-Host` para mostrar estados de variáveis ou o clássico método de tentativa e erro. Com o advento do PowerShell ISE, e mais recentemente, do VS Code com seus recursos de depuração avançados, a depuração no PowerShell se tornou quase tão intuitiva quanto em linguagens de programação completas.

Alternativas às ferramentas nativas de depuração do PowerShell incluem ferramentas de terceiros como PowerGUI ou o uso de IDEs robustas como Visual Studio com um plugin do PowerShell.

Ao implementar um depurador, considere o escopo do script, especialmente ao trabalhar com scripts ou módulos pontuais. Os pontos de interrupção podem ser baseados em condições, mudanças de variáveis ou baseados em linha, permitindo um controle preciso durante uma sessão de depuração.

Além disso, com a transição para o PowerShell Core (PowerShell multiplataforma), a depuração em grande parte passou para as mãos do VS Code, que oferece uma experiência consistente em diferentes plataformas.

## Veja Também
Para mais informações sobre depuração no PowerShell:
- [about_Debuggers](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_Debuggers)
