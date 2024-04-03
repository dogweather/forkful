---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:35.884371-07:00
description: "Como fazer: Trabalhar com TOML no VBA envolve analisar o arquivo TOML\
  \ para ler configura\xE7\xF5es ou defini\xE7\xF5es no seu projeto VBA. O VBA n\xE3\
  o tem suporte\u2026"
lastmod: '2024-03-13T22:44:46.437722-06:00'
model: gpt-4-0125-preview
summary: "Trabalhar com TOML no VBA envolve analisar o arquivo TOML para ler configura\xE7\
  \xF5es ou defini\xE7\xF5es no seu projeto VBA."
title: Trabalhando com TOML
weight: 39
---

## Como fazer:
Trabalhar com TOML no VBA envolve analisar o arquivo TOML para ler configurações ou definições no seu projeto VBA. O VBA não tem suporte nativo para TOML, então normalmente você usaria um analisador ou converteria os dados TOML para um formato que o VBA possa trabalhar facilmente, como JSON ou XML. Aqui está como analisar manualmente um arquivo de configuração TOML simples:

1. **Arquivo TOML de Exemplo** (`config.toml`):
```
title = "TOML Example"

[database]
server = "192.168.1.1"
ports = [ 8000, 8001, 8002 ]
connection_max = 5000
enabled = true
```

2. **Código VBA para Analisar TOML**:

Assumindo que o conteúdo TOML seja lido em uma variável de string `tomlStr`, o seguinte código VBA demonstra uma abordagem simplista para analisar a seção `[database]`:

```vb
Function ParseTOML(tomlStr As String)
    Dim lines() As String
    lines = Split(tomlStr, vbCrLf)
    
    Dim config As Object
    Set config = CreateObject("Scripting.Dictionary")
    Dim currentSection As String
    currentSection = ""
    
    Dim i As Integer
    For i = 0 To UBound(lines)
        Dim line As String
        line = Trim(lines(i))
        If InStr(line, "[") > 0 And InStr(line, "]") > 0 Then
            currentSection = Mid(line, 2, Len(line) - 2)
            Set config(currentSection) = CreateObject("Scripting.Dictionary")
        ElseIf InStr(line, "=") > 0 Then
            Dim parts() As String
            parts = Split(line, "=")
            Dim key As String
            key = Trim(parts(0))
            Dim value As String
            value = Trim(parts(1))
            config(currentSection)(key) = value
        End If
    Next i
    
    'Exemplo para acessar dados analisados
    Debug.Print "Database Server: "; config("database")("server")
End Function
```

3. **Saída de Exemplo** (Janela Imediata):
```
Database Server: 192.168.1.1
```

## Aprofundamento
A aceitação prática do TOML na comunidade de desenvolvedores mostra uma tendência para arquivos de configuração mais simples e legíveis para humanos, em contraste com o anteriormente prevalente XML. A filosofia de design do TOML enfatiza semânticas claras e visa uma análise direta com o mínimo de sobrecarga. No VBA, lidar diretamente com TOML envolve análise manual ou o aproveitamento de ferramentas externas para converter TOML em um formato mais amigável para o VBA, devido à falta de suporte nativo. Embora este método de análise manual mostre uma abordagem fundamental, a utilização de bibliotecas externas ou formatos intermediários como JSON pode oferecer estratégias de análise mais robustas e resistentes a erros. Dada a integração extensiva do VBA com o Microsoft Office, converter TOML para JSON e usar as capacidades nativas de análise de JSON do VBA (quando aplicável) ou analisadores de JSON de terceiros poderia proporcionar um fluxo de trabalho mais simplificado. Além disso, com a evolução contínua dos formatos de serialização de dados, os programadores também deveriam considerar YAML, que, como o TOML, enfatiza a legibilidade humana, mas oferece diferentes compensações em termos de complexidade e flexibilidade.
