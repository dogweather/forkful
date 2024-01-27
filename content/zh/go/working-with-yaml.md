---
title:                "处理 YAML 文件"
date:                  2024-01-19
html_title:           "Bash: 处理 YAML 文件"
simple_title:         "处理 YAML 文件"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
为什么要用YAML？简单：配置文件。YAML易读写，常用来配置。程序员用它因为它清晰、跨语言。

## How to:
安装YAML库：
```bash
go get gopkg.in/yaml.v3
```

Go中解析YAML：
```go
package main

import (
	"fmt"
	"gopkg.in/yaml.v3"
	"io/ioutil"
)

type Config struct {
	Title string `yaml:"title"`
	Owner struct {
		Name string `yaml:"name"`
	} `yaml:"owner"`
}

func main() {
	data, err := ioutil.ReadFile("config.yaml")
	if err != nil {
		panic(err)
	}
	var config Config
	err = yaml.Unmarshal(data, &config)
	if err != nil {
		panic(err)
	}
	fmt.Printf("Title: %s, Owner: %s\n", config.Title, config.Owner.Name)
}
```

样例config.yaml文件：
```yaml
title: Example YAML
owner:
  name: Zhang Wei
```

样例输出：
```bash
Title: Example YAML, Owner: Zhang Wei
```

## Deep Dive
历史: YAML诞生于2001，进化自XML 和 JSON；但人性化多了。
替代品: JSON 和 XML 是替代品，但YAML对人类更友好。
细节: YAML不允许制表符（Tab），要空格；缩进重要。

## See Also
- YAML 官网：https://yaml.org/
- Go yaml.v3 文档：https://pkg.go.dev/gopkg.in/yaml.v3
- YAML 与 JSON 对比：https://en.wikipedia.org/wiki/YAML#Comparison_with_JSON
