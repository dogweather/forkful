---
title:                "Go: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por que trabalhar com YAML em Go

Se você é um desenvolvedor Go iniciante, provavelmente já ouviu falar do YAML como uma linguagem de marcação de dados muito usada em aplicações web. Mas por que você deveria considerar utilizar YAML em seus projetos em Go? Existem algumas vantagens em trabalhar com YAML em comparação com outras linguagens de configuração.

Primeiramente, o formato YAML é escrita de forma legível para humanos, o que torna fácil de entender e editar manualmente para configurações de suas aplicações. Além disso, ele também é muito útil para manter diferentes configurações de ambientes de desenvolvimento, teste e produção em apenas um arquivo YAML.

## Como usar YAML em Go

Agora que você entendeu por que é importante trabalhar com YAML em projetos Go, vamos ver como utilizá-lo em seu código. O pacote 'gopkg.in/yaml.v2' é amplamente usado para fazer a leitura e escrita de arquivos YAML em Go. Veja um exemplo simples abaixo de como fazer a leitura de um arquivo YAML e acessar seus campos:

```Go
import (
	"fmt"
	"io/ioutil"
	"log"

	"gopkg.in/yaml.v2"
)

type Config struct {
	Server   string `yaml:"server"`
	Port     string `yaml:"port"`
	Username string `yaml:"username"`
	Password string `yaml:"password"`
}

func main() {
	config := Config{}
	yamlFile, err := ioutil.ReadFile("config.yaml")
	if err != nil {
		log.Fatalf("Erro ao ler arquivo YAML: %v", err)
	}
	err = yaml.Unmarshal(yamlFile, &config)
	if err != nil {
		log.Fatalf("Erro ao fazer a Unmarshal do arquivo YAML: %v", err)
	}
	fmt.Println("Server:", config.Server)
	fmt.Println("Port:", config.Port)
	fmt.Println("Username:", config.Username)
	fmt.Println("Password:", config.Password)
}
```

Saída:

```
Server: localhost
Port: 8080
Username: usuario
Password: senha
```

## Mergulho mais profundo em YAML

Existem muitas bibliotecas de terceiros que podem ser usadas para trabalhar com YAML em Go, algumas delas oferecem recursos avançados, como validação e concatenação de arquivos YAML. Também é possível incluir dados de forma dinâmica no arquivo YAML durante a execução do programa.

É importante lembrar que o pacote 'gopkg.in/yaml.v2' é a versão atualmente estável, mas o pacote 'sigs.k8s.io/yaml' é a versão futura e oferece recursos adicionais, como suporte a YAML com múltiplos documentos e suporte a anotações do Kubernetes.

## Veja também

- [Documentação oficial do pacote 'gopkg.in/yaml.v2'](https://pkg.go.dev/gopkg.in/yaml.v2)
- [Documentação oficial do pacote 'sigs.k8s.io/yaml'](https://pkg.go.dev/sigs.k8s.io/yaml)