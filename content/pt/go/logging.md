---
title:                "Registro de Logs"
date:                  2024-01-26T01:07:27.184379-07:00
model:                 gpt-4-1106-preview
simple_title:         "Registro de Logs"
programming_language: "Go"
category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/logging.md"
---

{{< edit_this_page >}}

## O quê e Por quê?
"Logging" é simplesmente manter um registro de eventos, estados e fluxos de dados dentro de um aplicativo. Programadores fazem isso para diagnosticar bugs, monitorar desempenho e acompanhar a saúde operacional do aplicativo—tornando-o praticamente o equivalente de software de uma caixa-preta em aviões.

## Como fazer:
Em Go, o registro de logs pode ser tratado de várias maneiras, desde o pacote padrão `log` da biblioteca padrão até bibliotecas de terceiros como `logrus` e `zap`. Aqui está um exemplo simples usando o pacote `log` embutido:

```Go
package main

import (
	"log"
	"os"
)

func main() {
	// Criar um arquivo de log
	logFile, err := os.OpenFile("app.log", os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0666)
	if err != nil {
		log.Fatal(err)
	}
	defer logFile.Close()

	// Definir a saída do log para o arquivo
	log.SetOutput(logFile)

	// Registrar alguns eventos
	log.Println("Iniciando a aplicação...")
	// ... lógica da aplicação aqui ...
	log.Println("Aplicação encerrada com sucesso.")
}
```

Se você executar este código, não verá nenhuma saída no terminal, porque tudo está indo para `app.log`. Aqui está uma olhada no que você encontraria dentro desse arquivo de log:

```
2023/01/02 15:04:05 Iniciando a aplicação...
2023/01/02 15:05:01 Aplicação encerrada com sucesso.
```

## Aprofundando
O registro de logs em programação remonta aos primeiros computadores, onde os engenheiros literalmente encontravam bugs (mariposas, para ser exato) esmagadas no hardware, e eles registravam isso! Avançando para hoje, e o registro de logs tornou-se uma forma sofisticada de entender o que está acontecendo dentro de sistemas complexos.

Enquanto o pacote `log` em Go é bastante simplista, ele pode ser suficiente para aplicações básicas. No entanto, no contexto de sistemas distribuídos modernos, ou quando você precisa de um controle mais matizado sobre a sua saída de log (como diferentes níveis de severidade), você pode querer explorar soluções mais robustas.

Bibliotecas de terceiros como `logrus` e `zap` oferecem registro de logs estruturado, o que significa que você pode registrar tipos de dados complexos como JSON, tornando mais fácil interpretar logs, especialmente em conjunto com sistemas de gerenciamento de logs como ELK Stack ou Splunk.

Ao considerar a implementação de uma estratégia de registro de logs, também é essencial pensar sobre as implicações de desempenho. Bibliotecas de registro de logs de alto desempenho são otimizadas para reduzir o impacto na taxa de transferência e latência da aplicação. Por exemplo, `zap` se gaba de seu design rápido e com baixa alocação, o que pode ser crucial para sistemas em tempo real.

Além de várias bibliotecas, formatos e padrões de registro de logs também valem a pena ser notados. Formatos de registro de logs estruturados como JSON podem ser imensamente poderosos quando usados em conjunto com sistemas de processamento de logs. Por outro lado, logs em texto simples são legíveis por humanos, mas mais desafiadores para serem analisados programaticamente.

## Veja Também
Para se aprofundar nas capacidades de registro de logs em Go, esses recursos podem ser úteis:

- O blog do Go sobre registro de logs: https://blog.golang.org/logging
- `logrus`, um registrador estruturado para Go: https://github.com/sirupsen/logrus
- `zap`, um registrador estruturado e rápido: https://github.com/uber-go/zap
- ELK Stack (Elasticsearch, Logstash, Kibana) para análise de logs: https://www.elastic.co/what-is/elk-stack
- Uma comparação de bibliotecas de registro de logs em Go: https://www.loggly.com/blog/benchmarking-5-popular-golang-logging-libraries/
