---
date: 2024-01-27 16:21:01.503705-07:00
description: "Manipular arquivos com comandos de uma linha no PowerShell \xE9 sobre\
  \ alterar, mover, ou obter dados de arquivos diretamente da linha de comando de\
  \ forma\u2026"
lastmod: 2024-02-19 22:05:05.848130
model: gpt-4-0125-preview
summary: "Manipular arquivos com comandos de uma linha no PowerShell \xE9 sobre alterar,\
  \ mover, ou obter dados de arquivos diretamente da linha de comando de forma\u2026"
title: Manipulando arquivos com one-liners de CLI
---

{{< edit_this_page >}}

## O que & Por quê?

Manipular arquivos com comandos de uma linha no PowerShell é sobre alterar, mover, ou obter dados de arquivos diretamente da linha de comando de forma rápida. Programadores fazem isso por eficiência; é mais rápido do que navegar por interfaces gráficas ou escrever grandes scripts para tarefas simples.

## Como fazer:

### Lendo um Arquivo
Para exibir rapidamente o conteúdo de um arquivo, use o comando `Get-Content`:
```PowerShell
Get-Content .\example.txt
```

### Escrevendo em um Arquivo
Para escrever algo novo em um arquivo, `Set-Content` pode ser usado:
```PowerShell
Set-Content -Path .\example.txt -Value "Hello, PowerShell!"
```

### Acrescentando a um Arquivo
Acrescentar dados ao final de um arquivo sem apagar seu conteúdo pode ser feito com `Add-Content`:
```PowerShell
Add-Content -Path .\example.txt -Value "Adding this line."
```

### Copiando Arquivos
Copiar um arquivo é simples com `Copy-Item`:
```PowerShell
Copy-Item -Path .\example.txt -Destination .\copy_of_example.txt
```

### Deletando Arquivos
Para remover um arquivo, simplesmente use `Remove-Item`:
```PowerShell
Remove-Item -Path .\unwanted_file.txt
```

### Pesquisando Dentro de Arquivos
Use `Select-String` para procurar texto dentro de arquivos:
```PowerShell
Select-String -Path .\*.txt -Pattern "PowerShell"
```

### Combinando Comandos
O PowerShell realmente brilha com sua capacidade de encadear comandos usando pipes. Aqui está como você pode encontrar arquivos e copiá-los para um novo diretório:
```PowerShell
Get-ChildItem -Path .\*.log | Copy-Item -Destination C:\Logs
```

## Mergulho Profundo

Historicamente, o PowerShell foi introduzido como uma alternativa mais poderosa ao prompt de comando tradicional no Windows, oferecendo acesso sem precedentes aos internos do sistema e armazéns de dados. Ele combina a velocidade da linha de comando com a flexibilidade da scriptagem, tornando-se uma ferramenta inestimável para administradores de sistemas baseados em Windows e desenvolvedores.

Alternativas ao PowerShell para manipulação de arquivos incluem ferramentas baseadas em Unix como `sed`, `awk`, `grep`, e scriptagem `bash` para usuários de Linux e MacOS. Enquanto essas ferramentas são extremamente poderosas e têm seus próprios méritos, o PowerShell oferece uma integração profunda com ambientes Windows.

Um aspecto notável do PowerShell é sua natureza orientada a objetos. Diferente de muitas linguagens de script que tratam tudo como strings ou fluxos de bytes, o PowerShell trabalha diretamente com objetos do .NET. Isso significa que, quando você manipula arquivos, está trabalhando com objetos ricos que fornecem uma infinidade de propriedades e métodos, tornando tarefas complexas mais gerenciáveis.

Uma das fraquezas do PowerShell, particularmente para usuários Linux e MacOS, é sua verbosidade percebida em comparação com scriptagem bash ou uso de ferramentas de linha de comando Unix. Além disso, a integração profunda do PowerShell com o Windows pode às vezes tornar scripts multiplataforma um tanto desafiadores, embora esforços com o PowerShell Core visem efetivamente fechar essa lacuna.

Independentemente de suas fraquezas, a força do PowerShell reside em suas poderosas capacidades de uma linha, ambiente de scriptagem integrado, e o acesso abrangente que oferece ao ecossistema Windows, tornando-se uma ferramenta essencial para aqueles que procuram manipular arquivos e muito mais diretamente da linha de comando.
