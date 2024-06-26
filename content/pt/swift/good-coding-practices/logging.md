---
date: 2024-01-26 01:09:09.404601-07:00
description: "Como fazer: Em Swift, voc\xEA pode escrever logs no console com instru\xE7\
  \xF5es de impress\xE3o ou com a API `os.log`, mais flex\xEDvel, que se integra ao\
  \ Unified\u2026"
lastmod: '2024-03-13T22:44:46.926728-06:00'
model: gpt-4-1106-preview
summary: "Em Swift, voc\xEA pode escrever logs no console com instru\xE7\xF5es de\
  \ impress\xE3o ou com a API `os.log`, mais flex\xEDvel, que se integra ao Unified\
  \ Logging System nas plataformas da Apple."
title: Registro de Logs
weight: 17
---

## Como fazer:
Em Swift, você pode escrever logs no console com instruções de impressão ou com a API `os.log`, mais flexível, que se integra ao Unified Logging System nas plataformas da Apple.

``` Swift
import os.log

let logger = OSLog(subsystem: "com.seuapp.dominio", category: "network")

func fetchData() {
    // Instrução de impressão simples
    print("Início da busca")
    
    // Registrando evento de nível de informação usando os.log
    os_log(.info, log: logger, "Buscando dados da API.")
    
    do {
        let data = try performNetworkRequest()
        // Registrando evento de nível de depuração
        os_log(.debug, log: logger, "Dados recebidos: %@", data.description)
    } catch {
        // Registrando evento de nível de erro
        os_log(.error, log: logger, "Falha ao buscar dados: %@", error.localizedDescription)
    }
}

func performNetworkRequest() throws -> Data {
    // Simula uma solicitação de rede
    return Data()
}
```

Um exemplo de saída no console pode parecer com isso:

```
Início da busca
Buscando dados da API.
Dados recebidos: Alguns bytes de dados...
```

Para erros, pode ser:

```
Falha ao buscar dados: A conexão com a Internet parece estar offline.
```

## Aprofundamento
O registro de logs em Swift ganha nova força e eficiência com o Unified Logging System introduzido no iOS 10 e macOS Sierra. Ao contrário da instrução `print` que vai direto para o console, esse sistema é baseado em atividades, e permite que você filtre mensagens de log com base na sua importância e se são construções de depuração ou de lançamento.

O contexto histórico enquadra a evolução do registro de logs no iOS e macOS, de instruções de impressão rudimentares para ferramentas abrangentes que se integram ao aplicativo Instruments e ao Console, fornecendo maneiras sofisticadas de analisar logs.

Existem uma série de alternativas para registro de logs dentro do Swift, como bibliotecas de terceiros como CocoaLumberjack, que oferece uma camada de macro sobre o Unified Logging System. Ela fornece controle aprimorado sobre a formatação de logs, gerenciamento de arquivos e opções de desempenho.

Por fim, detalhes de implementação; OSLog é projetado não apenas para ser eficiente, mas também consciente da privacidade, com a capacidade de ofuscar dados privados ao registrar logs. Ele categoriza logs em níveis de falha, erro, informação e depuração, cada um oferecendo uma granularidade diferente para resolução de problemas.

## Veja Também
- [Documentação do Unified Logging da Apple](https://developer.apple.com/documentation/os/logging)
- [Tutorial de registro de logs do Ray Wenderlich](https://www.raywenderlich.com/605079-logging-in-swift-oslog)
- [Repositório GitHub do CocoaLumberjack](https://github.com/CocoaLumberjack/CocoaLumberjack)
