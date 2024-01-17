---
title:                "Verificando se um diretório existe"
html_title:           "PowerShell: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O que e por que?

Verificar se um diretório existe é um procedimento comum em programação para determinar a existência de uma pasta específica em um sistema de arquivos. Isso é importante para garantir que o código funcione corretamente e evite erros quando é esperado que o diretório exista.

## Como fazer:

A verificação da existência de um diretório pode ser feita usando o cmdlet `Test-Path` do PowerShell. Basta fornecer o caminho do diretório que deseja verificar como argumento. Por exemplo, para verificar se o diretório "Documentos" existe no seu usuário atual, você pode usar o seguinte comando:

```PowerShell
Test-Path C:\Usuários\Nome\Documentos
```

Se o diretório existir, o comando retornará "True". Caso contrário, se o diretório não existir ou se houver algum erro, o comando retornará "False". Você também pode usar o parâmetro `-PathType` para especificar se o diretório deve ser um diretório, um arquivo ou ambos.

## Dica Pro:

Você também pode usar a expansão de caminho('~') para verificar diretórios no diretório inicial do usuário atual. O comando seria semelhante ao seguinte:

```PowerShell
Test-Path ~\Documentos
```

## Detalhando:

- Contexto histórico: Verificar a existência de um diretório é uma tarefa comum em sistemas operacionais devido à estrutura de arquivos hierárquica. Em versões anteriores do PowerShell, o cmdlet `Exist` era usado para essa finalidade.

- Alternativas: Além do PowerShell, existem outras formas de verificar a existência de diretórios em diferentes linguagens de programação, como Python, Java e C#. Cada uma dessas linguagens possui suas próprias funções ou métodos para isso.

- Detalhes de implementação: Usando o cmdlet `Test-Path`, o PowerShell realiza uma consulta no sistema de arquivos para verificar se o diretório existe. Ele também suporta wildcards, o que significa que você pode verificar a existência de vários diretórios usando um único comando.

## Veja também:

- Documentação oficial do cmdlet `Test-Path`: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/test-path
- Usando wildcards no cmdlet `Test-Path`: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/test-path