---
title:                "Registro de Logs"
date:                  2024-01-26T01:07:17.980953-07:00
model:                 gpt-4-1106-preview
simple_title:         "Registro de Logs"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/logging.md"
---

{{< edit_this_page >}}

## O Quê e Por Quê?
Registrar é basicamente deixar um rastro de migalhas através do seu código - é como você acompanha o que está acontecendo quando o seu script está rodando solto. Programadores registram para depurar, para rastrear o comportamento do aplicativo, para monitorar o desempenho e para ficar de olho em qualquer travessura.

## Como fazer:
Aqui vai a essência de como adicionar um registro básico aos seus scripts:

```PowerShell
# Criando uma mensagem de log simples
Write-Host "Info: Iniciando o processo do script."

# Escrevendo em um arquivo
"Info: Esta é uma mensagem registrada." | Out-File -Append myLog.log

# Usando o cmdlet embutido para um registro mais detalhado
Start-Transcript -Path "./detailedLog.log"
Write-Output "Atenção: Algo não está totalmente correto."
# ... seu script faz coisas
Stop-Transcript

# Saída de detailedLog.log
******************************
Transcrição do Windows PowerShell iniciada
Hora de início: 20230324112347
Usuário   : PShellGuru@example.com
Usuário como Admin: PShellGuru@example.com
Nome da Configuração: 
Máquina  : PS-DEVBOX (Microsoft Windows NT 10.0.17763.0)
Aplicativo Host: C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe
ID do Processo: 2024
Versão do PS: 7.1.2
```

Agora, nos seus registros, há um relato detalhado do que o seu código tem feito.

## Aprofundando:
Historicamente, o registro é tão antigo quanto a programação em si. É como o diário de bordo de um capitão, mas para software. No passado, poderiam ter sido impressões ou máquinas de teletipo; agora, trata-se de arquivos e sofisticados sistemas de gerenciamento de registros.

Quando você está na trincheira do PowerShell, `Write-Host` é rápido e direto, mas apenas expele texto para o console, não é ótimo para manter registros. `Out-File` oferece uma maneira simples de jogar texto em um arquivo, mas para a informação real, você vai querer `Start-Transcript` e `Stop-Transcript`, que registram tudo — entrada, saída, o pacote completo.

Alternativas? Claro, se você está gerenciando uma empresa, você pode olhar para o Log de Eventos do Windows ou usar softwares como Logstash, mas para o seu script diário, fique com as ferramentas do PowerShell. Quanto à implementação, lembre-se de registrar de forma inteligente – muito pouco e é inútil, demais e é apenas ruído branco.

## Veja Também:
Confira estes para ter um controle sobre tudo em registro no PowerShell: