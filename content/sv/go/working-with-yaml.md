---
title:                "Go: Att arbeta med yaml"
simple_title:         "Att arbeta med yaml"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/working-with-yaml.md"
---

{{< edit_this_page >}}

# Varför

I många moderna applikationer, speciellt inom moln- och containerbaserad utveckling, används YAML-filer för att konfigurera och definiera applikationer och dess miljöer. Att kunna arbeta med YAML-filer är en viktig färdighet för att kunna hantera dessa applikationer på ett effektivt sätt. I denna bloggpost kommer vi att utforska hur man kan arbeta med YAML i Go-programmering för att underlätta hanteringen av applikationer och dess konfigurationer.

# Hur man gör det

För att arbeta med YAML i Go-programmering behöver vi importera "gopkg.in/yaml.v2" paketet. Sedan kan vi använda funktioner som Unmarshal() för att läsa in YAML-filer och Marshal() för att skapa nya YAML-filer.

```Go
import (
	"fmt"
	"gopkg.in/yaml.v2"
)

type Application struct {
	Name      string   `yaml:"name"`
	Ports     []int    `yaml:"ports"`
	Environments struct {
		Development string `yaml:"dev"`
		Production  string `yaml:"prod"`
	} `yaml:"environments"`
}

func main() {
	yamlString := `
        name: MyApplication
        ports:
          - 8080
          - 8081
        environments:
          dev: development
          prod: production
    `
	var myApp Application
	err := yaml.Unmarshal([]byte(yamlString), &myApp)
	if err != nil {
		panic(err)
	}
	fmt.Println("Application Name: ", myApp.Name)
	fmt.Println("Ports: ", myApp.Ports)
	fmt.Println("Development Environment: ", myApp.Environments.Development)
	fmt.Println("Production Environment: ", myApp.Environments.Production)

	// Output:
	// Application Name: MyApplication
	// Ports: [8080 8081]
	// Development Environment: development
	// Production Environment: production
}
```

## Fördjupning

För att kunna arbeta mer effektivt med YAML i Go-programmering, kan vi använda strukturer för att definiera vår YAML-data. Detta gör det lättare att läsa och hantera data från YAML-filer.

```Go
import (
	"fmt"
	"gopkg.in/yaml.v2"
	"io/ioutil"
)

// Struct för att definiera vår YAML-data
type AppConfig struct {
	Version string   `yaml:"version"`
	Services []Service `yaml:"services"`
}

type Service struct {
	Name string `yaml:"name"`
	Ports []int `yaml:"ports"`
}

func main() {
	// Läs in YAML-filen
	yamlFile, err := ioutil.ReadFile("app_config.yaml")
	if err != nil {
		panic(err)
	}

	// Skapa en ny instans av vår strukt med YAML-data
	var appConfig AppConfig
	err = yaml.Unmarshal(yamlFile, &appConfig)
	if err != nil {
		panic(err)
	}

	// Loopa igenom alla services och skriv ut namn och portar
	for _, service := range appConfig.Services {
		fmt.Println(service.Name, " Ports: ", service.Ports)
	}

	// Output:
	// Database  Ports: [3306]
	// WebServer  Ports: [80 443]
}

```

## Se även

Här är några länkar som kan vara användbara för att lära dig mer om att arbeta med YAML i Go-programmering:

- [YAML-paket för Go](https://github.com/go-yaml/yaml)
- [Go dokumentationen för YAML-paketet](https://pkg.go.dev/gopkg.in/yaml.v2)
- [En guide för att arbeta med YAML i Go](https://www.onlinetutorialspoint.com/golang/yaml-handling-with-go-language.html)

Lycka till med att utforska YAML i Go och hör gärna av dig om du har några frågor eller feedback!