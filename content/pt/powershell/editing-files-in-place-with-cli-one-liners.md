---
title:                "Editando arquivos in loco com comandos de linha de comando"
date:                  2024-01-27T16:20:40.576630-07:00
model:                 gpt-4-0125-preview
simple_title:         "Editando arquivos in loco com comandos de linha de comando"

category:             "PowerShell"
tag:                  "Data and Text Processing"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/editing-files-in-place-with-cli-one-liners.md"
---

{{< edit_this_page >}}

## O Que & Por Que?

Editar arquivos no local com comandos de linha de comando (CLI) em uma linha no PowerShell é sobre fazer modificações diretas nos arquivos a partir da linha de comando, sem a necessidade de abri-los em um editor. Esta abordagem economiza tempo e pode ser particularmente útil para o processamento em lote ou automação de tarefas de edição repetitivas em múltiplos arquivos.

## Como fazer:

### Substituindo Texto em um Único Arquivo

Vamos começar com uma tarefa simples: você quer substituir todas as instâncias de "oldtext" por "newtext" em um arquivo chamado example.txt. Aqui está como você faria isso:

```PowerShell
(Get-Content example.txt) -replace 'oldtext', 'newtext' | Set-Content example.txt
```

Este comando lê o conteúdo, realiza a substituição e escreve o conteúdo de volta no arquivo original.

### Editando Múltiplos Arquivos

E se você precisar aplicar a mesma mudança em vários arquivos? Aqui está uma abordagem usando um loop:

```PowerShell
Get-ChildItem *.txt | ForEach-Object {
  (Get-Content $_) -replace 'oldtext', 'newtext' | Set-Content $_
}
```

Este trecho de código encontra todos os arquivos `.txt` no diretório atual, substituindo "oldtext" por "newtext" em cada um deles.

### Adicionar Conteúdo no Início ou no Final dos Arquivos

Anexar ou prepor conteúdo também pode ser simplificado:

```PowerShell
# Prepondo
"New first line`n" + (Get-Content example.txt) | Set-Content example.txt

# Anexando
(Get-Content example.txt) + "`nNew last line" | Set-Content example.txt
```

Aqui, simplesmente concatenamos o novo conteúdo antes ou depois do conteúdo existente e salvamos novamente.

## Aprofundamento

Historicamente, a edição no local é mais comumente associada a ferramentas Unix como `sed` e `awk`. O PowerShell, sendo um participante mais recente, não inclui um recurso dedicado de edição no local imediatamente disponível. Isso se deve, em parte, à sua filosofia de design, destacando a importância de objetos sobre fluxos de texto, ao contrário das ferramentas Unix que tratam a maioria das entradas como texto.

Alternativas ao PowerShell para esta tarefa incluem o uso das tradicionais ferramentas Unix disponíveis no Windows por meio do Cygwin ou do Subsistema Windows para Linux (WSL). Estas ferramentas frequentemente fornecem uma sintaxe mais concisa para edição no local devido ao seu design centrado em texto.

Em termos de implementação, é importante notar que a abordagem do PowerShell envolve ler o arquivo inteiro na memória, fazer as mudanças, e depois escrevê-lo de volta. Embora isso funcione bem para arquivos de tamanho moderado, pode se tornar ineficiente para arquivos muito grandes. Nesses casos, pode-se considerar o uso direto dos métodos `.NET` ou recorrer a ferramentas alternativas projetadas para transmitir grandes volumes de dados.

Apesar dessas considerações, a flexibilidade do PowerShell e seu extenso conjunto de recursos o tornam uma ferramenta inestimável para manipulação de arquivos diretamente da linha de comando, especialmente para aqueles já enraizados no ecossistema Windows ou gerenciando ambientes multiplataforma.
