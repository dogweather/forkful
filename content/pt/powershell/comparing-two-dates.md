---
title:                "Comparando duas datas"
html_title:           "PowerShell: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O que é e por que fazer? 

A comparação de duas datas é uma tarefa comum na programação, que envolve verificar se duas datas são iguais, anteriores ou posteriores uma à outra. Os programadores fazem isso para validar entradas do usuário, realizar cálculos de tempo e agendar tarefas.

## Como fazer:

```PowerShell
# Comparando datas
$date1 = Get-Date -Date "01/01/2020"
$date2 = Get-Date -Date "02/01/2020"

# Verificando se $date1 e $date2 são iguais
if ($date1 -eq $date2) {
    Write-Host "As datas são iguais!"
}

# Verificando se $date1 acontece antes de $date2
if ($date1 -lt $date2) {
    Write-Host "A primeira data ocorre antes da segunda data."
}

# Verificando se $date1 acontece depois de $date2
if ($date1 -gt $date2) {
    Write-Host "A primeira data ocorre depois da segunda data."
}
```

Exemplo de saída no terminal:

```
As datas são iguais!
A primeira data ocorre antes da segunda data.
```

## Mais informações:

- Contexto histórico: A tarefa de comparação de datas tornou-se mais fácil com o avanço da tecnologia, pois antes era necessário realizar cálculos manuais para comparar datas.
- Alternativas: Além do PowerShell, existem outras linguagens que permitem comparar datas, como C#, JavaScript e Python.
- Detalhes de implementação: Ao comparar datas, é importante garantir que elas estejam no mesmo formato (ex: dia/mês/ano ou ano-mês-dia), caso contrário a comparação pode retornar resultados errados.

## Veja também:

- [Documentação oficial do PowerShell](https://docs.microsoft.com/en-us/powershell/)
- [Artigo sobre como comparar datas em C#](https://www.infoworld.com/article/3036570/how-to-compare-dates-in-c.html)
- [Tutorial sobre comparar datas em JavaScript](https://www.digitalocean.com/community/tutorials/how-to-compare-dates-in-javascript)