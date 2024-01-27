---
title:                "Modificando arquivos com comandos de uma linha no terminal"
date:                  2024-01-26T22:25:17.071319-07:00
model:                 gpt-4-0125-preview
simple_title:         "Modificando arquivos com comandos de uma linha no terminal"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data and Text Processing"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/modifying-files-with-cli-one-liners.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Modificar arquivos usando Interface de Linha de Comando (CLI) com comandos únicos no PowerShell é sobre usar comandos sucintos para editar, transformar ou atualizar arquivos diretamente do terminal. Programadores fazem isso para rapidamente fazer alterações nos arquivos sem abri-los em um editor gráfico, acelerando o fluxo de trabalho e permitindo a automação de tarefas repetitivas.

## Como fazer:

Para substituir uma string específica em um arquivo, você pode usar os cmdlets `Get-Content` e `Set-Content` combinados com o cmdlet `ForEach-Object`, assim:

```PowerShell
Get-Content ./example.txt | ForEach-Object { $_ -replace 'oldString', 'newString' } | Set-Content ./example.txt
```

Para adicionar uma linha ao final de um arquivo, você pode usar o cmdlet `Add-Content`:

```PowerShell
Add-Content ./example.txt "Esta é a nova linha no final do arquivo."
```

Suponha que você queira remover linhas em branco de um arquivo. Nesse caso, o PowerShell torna isso direto:

```PowerShell
Get-Content ./example.txt | Where-Object { $_.Trim() -ne '' } | Set-Content ./cleaned_example.txt
```

E a amostra de saída para a remoção de linhas em branco pode simplesmente ser o conteúdo de `cleaned_example.txt` agora excluindo quaisquer das linhas vazias ou apenas com espaços em branco que estavam presentes em `example.txt`.

## Mergulho Profundo

O poder de modificar arquivos com comandos únicos CLI no PowerShell é enraizado em seu conjunto abrangente de cmdlets, que são construídos sobre o framework .NET, dando-lhe um robusto conjunto de capacidades. Este método remonta à filosofia Unix de criar ferramentas simples que fazem um trabalho bem feito, um princípio que o PowerShell expande ao fornecer um kit de ferramentas versátil dentro de um único shell.

Alternativas ao PowerShell para esta tarefa incluem o uso de ferramentas baseadas em Unix como `sed`, `awk` ou `grep` em ambientes como Bash. Estas ferramentas são altamente eficientes e têm sido a solução preferida para manipulação de arquivos em sistemas Unix/Linux por décadas. No entanto, a abordagem do PowerShell, que se integra de forma estreita com o Modelo de Objeto do Windows, oferece uma vantagem única em ambientes Windows.

Um detalhe de implementação significativo a ser notado é que o PowerShell processa o conteúdo do arquivo na memória, o que o torna menos eficiente para arquivos muito grandes em comparação com algumas ferramentas orientadas para stream em Unix/Linux. Além disso, a verbosidade do PowerShell, embora torne os scripts legíveis, às vezes pode levar a comandos únicos mais longos em comparação com seus equivalentes Unix. No entanto, para ambientes e tarefas centrados no Windows e que se beneficiam da integração profunda com o ecossistema Windows, o PowerShell oferece capacidades inigualáveis.

## Veja Também

Para leitura adicional e exemplos mais complexos de manipulação de arquivos no PowerShell, você pode achar esses recursos úteis:

- A documentação oficial do PowerShell, que fornece um guia abrangente para seus cmdlets: [https://docs.microsoft.com/pt-br/powershell/](https://docs.microsoft.com/pt-br/powershell/)
- "Guia de Scripting do PowerShell" por Ed Wilson, que oferece discussões aprofundadas e exemplos sobre scripting, incluindo tarefas de manipulação de arquivos.
- Para aqueles interessados em compatibilidade cruzada ou vindos de um background Unix, "Aprendendo PowerShell para Admins de Linux" é um excelente recurso para entender o poder do PowerShell através dos diferentes sistemas operacionais.
