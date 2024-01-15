---
title:                "Trabalhando com yaml"
html_title:           "Go: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por que usar YAML em programação?

Se você está procurando uma maneira fácil de organizar e armazenar dados em seu código, YAML pode ser a solução perfeita para você! Com sua sintaxe simples e legibilidade humana, YAML é amplamente utilizado em diversas linguagens de programação, incluindo a atual versão do Go.

## Como usar YAML em seu código Go

Usar YAML em seu código Go é bastante simples. Primeiro, é importante ter o pacote "yaml" importado em seu arquivo. Em seguida, podemos criar um struct em Go que corresponda à estrutura de dados desejada em YAML. Por exemplo:

```Go
import (
	"fmt"
	"log"

	"gopkg.in/yaml.v2"
)

type MalhaDeOnibus struct {
	Linhas []string `yaml:"linhas"`
}

func main() {
	dadosYAML := `
linhas:
  - 001: Rio Comprido
  - 002: Tijuca
  - 003: Copacabana
`
	var malha MalhaDeOnibus
	err := yaml.Unmarshal([]byte(dadosYAML), &malha)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("%+v", malha)
}
```

O código acima criará uma estrutura de dados em Go a partir dos dados YAML fornecidos. Como saída, teremos:

```
{Linhas:[001: Rio Comprido 002: Tijuca 003: Copacabana]}
```

Agora, podemos acessar facilmente os valores individuais dentro dessa estrutura de dados em nosso código Go. Por exemplo, para obter a primeira linha de ônibus, podemos escrever:

```
malha.Linhas[0]
```

## Aprofundando no uso de YAML em Go

Além da estrutura de dados apresentada acima, também é possível trabalhar com YAML em Go de outras maneiras, como:

- Codificar e decodificar YAML em outros tipos de dados, como mapas ou slices;
- Trabalhar com a configuração de aplicativos em arquivos YAML;
- Utilizar tags para mapear diferentes tipos de dados em sua estrutura de dados em Go.

Para saber mais sobre todas as opções de uso de YAML em Go, confira a documentação oficial do pacote YAML para Go em [https://gopkg.in/yaml.v2](https://gopkg.in/yaml.v2).

## Veja também

- [Documentação oficial do pacote YAML para Go](https://gopkg.in/yaml.v2)
- [Tutorial sobre o uso de YAML em Go](https://medium.com/@matheusrodriguesdeoliveira/trabalhando-com-yaml-no-go-321ccf54c7af)
- [Exemplos práticos de uso de YAML em Go](https://github.com/go-yaml/yaml/tree/master/example)