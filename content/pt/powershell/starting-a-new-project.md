---
title:                "Iniciando um novo projeto"
html_title:           "Javascript: Iniciando um novo projeto"
simple_title:         "Iniciando um novo projeto"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Começar um novo projeto no PowerShell: Tudo o que você precisa saber

## Que é e por quê?

Começar um novo projeto em programação é o processo de estabelecer uma estrutura básica onde ficará seu código. É necessário para que possamos manter o código organizado, escalável e fácil de manter.

## Como fazer: 
Irei mostrar uma maneira simples de iniciar um novo projeto no PowerShell. 

```powershell
# Criar um novo diretório para o projeto
New-Item -ItemType Directory -Path C:\meu_projeto

# Navegar para o novo diretório
Set-Location -Path C:\meu_projeto

# Criar um novo arquivo de script PowerShell
New-Item -ItemType File -Name "meu_script.ps1"
```
E pronto! Agora você tem um novo projeto com um arquivo de script PowerShell.

## Mergulho Profundo

### Contexto Histórico
O PowerShell foi projetado pela Microsoft para automatizar o gerenciamento de tarefas, especialmente as tarefas de administração de sistemas. Ele utiliza uma linguagem de script crível e fazer melhorias contínua desde o seu lançamento em 2006.

### Alternativas
Existem outras ferramentas que podem ser usadas para iniciar um novo projeto, como Python, Node.js e Java. A escolha depende de vários fatores, incluindo requisitos do projeto, preferência pessoal e experiência.

### Detalhes de implementação
Ao iniciar um novo projeto no PowerShell, é uma boa prática seguir os princípios DRY(Don't Repeat Yourself). Para isso, podemos usamos funções e modulos no nosso código, tornando-o mais organizado.

## Veja Também

Se você quiser conhecer mais sobre a programação em PowerShell, sugiro os seguintes recursos:
- Documentação do PowerShell da Microsoft: [https://docs.microsoft.com/en-us/powershell/](https://docs.microsoft.com/en-us/powershell/)
- PowerShell para iniciantes: [https://docs.microsoft.com/en-us/powershell/scripting/overview?view=powershell-7](https://docs.microsoft.com/en-us/powershell/scripting/overview?view=powershell-7)