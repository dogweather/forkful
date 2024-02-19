---
aliases:
- /pt/powershell/starting-a-new-project/
date: 2024-01-20 18:04:26.632335-07:00
description: "Iniciar um novo projeto em PowerShell \xE9 como abrir um livro branco;\
  \ voc\xEA cria um espa\xE7o para desenvolver scripts e solu\xE7\xF5es automatizadas.\
  \ Programadores\u2026"
lastmod: 2024-02-18 23:08:58.369448
model: gpt-4-1106-preview
summary: "Iniciar um novo projeto em PowerShell \xE9 como abrir um livro branco; voc\xEA\
  \ cria um espa\xE7o para desenvolver scripts e solu\xE7\xF5es automatizadas. Programadores\u2026"
title: Iniciando um novo projeto
---

{{< edit_this_page >}}

## O Que & Porquê?
Iniciar um novo projeto em PowerShell é como abrir um livro branco; você cria um espaço para desenvolver scripts e soluções automatizadas. Programadores fazem isso para organizar código, facilitar a manutenção e colaboração, e definir claramente o escopo e intenção das suas ferramentas.

## Como Fazer:
Para começar um novo projeto em PowerShell, primeiro pense na estrutura de diretórios. Veja um exemplo simples de criação de diretório de projeto:

```PowerShell
New-Item -Path "C:\MeuProjeto" -ItemType Directory
cd C:\MeuProjeto
New-Item -Path ".\src" -ItemType Directory
New-Item -Path ".\tests" -ItemType Directory
echo '# Meu Novo Projeto PowerShell' > README.md
```

Resultado esperado depois de rodar o script:

```
Diretório: C:\MeuProjeto
Mode                LastWriteTime         Length Name
----                -------------         ------ ----
d-----        1/1/2023   12:00 PM                src
d-----        1/1/2023   12:00 PM                tests
-a----        1/1/2023   12:00 PM              0 README.md
```

## Aprofundando:
Historicamente, a criação de um novo projeto em PowerShell era menos estruturada: os scripts eram frequentemente desenvolvidos e armazenados ad-hoc. Com o PowerShell ganhando maturidade e a comunidade crescendo, práticas de desenvolvimento como versionamento (usando git), testes automatizados (usando Pester) e design modular tornaram-se comuns.

Alternativas incluem o uso de plataformas como o Visual Studio Code com extensões para PowerShell, que facilitam a configuração de projetos com templates e snipets de código.

Detalhes importantes na implementação de um projeto PowerShell incluem:

- Estruturação de pastas: por exemplo, separar código-fonte de testes.
- Scripts de build e deployment: automatizar a distribuição do seu projeto.
- Documentação e comentários: essenciais para a manutenção e expansão do código.

## Veja Também:
- Documentação oficial do PowerShell: https://docs.microsoft.com/powershell/
- Guia sobre estruturas de projeto: https://github.com/PoshCode/PowerShellPracticeAndStyle
- Extensão PowerShell para Visual Studio Code: https://marketplace.visualstudio.com/items?itemName=ms-vscode.PowerShell
- Tutorial sobre Pester para testes: https://pester.dev/docs/quick-start
