---
title:                "yaml 작업하기"
html_title:           "Go: yaml 작업하기"
simple_title:         "yaml 작업하기"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## 무엇인가요? 
YAML을 다루는 것은 단순한 데이터 형식으로 구조화된 파일을 작성하는 것입니다. 프로그래머들은 이를 효율적으로 사용할 수 있기 때문에 YAML을 사용합니다.

## 어떻게 하나요? 
```Go
import "gopkg.in/yaml.v2"

type Person struct {
  Name string `yaml:"name"`
  Age  int    `yaml:"age"`
}

data := `
name: John
age: 30
`

var person Person

err := yaml.Unmarshal([]byte(data), &person)
if err != nil {
    panic(err)
}

fmt.Printf("Name: %s, Age: %d", person.Name, person.Age)
// Output: Name: John, Age: 30
```

## 깊이 파고들기 
YAML은 2001년에 개발되었으며, JSON이나 XML과 같은 다른 데이터 형식보다 더 간단하고 가독성이 좋습니다. 대안으로는 TOML이나 HCL 같은 다른 데이터 형식이 있습니다. Go에서 YAML을 다루는 라이브러리는 gopkg.in/yaml.v2에 위치해 있습니다.

## 관련 자료 
- YAML 공식 홈페이지: https://yaml.org/
- Go에서 YAML 다루는 라이브러리: https://gopkg.in/yaml.v2