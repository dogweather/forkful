---
title:                "YAML 다루기"
date:                  2024-01-19
simple_title:         "YAML 다루기"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
(YAML 작업이란 무엇이며, 왜 프로그래머가 이것을 사용하나요?)

YAML은 데이터 직렬화 형식입니다. 설정 파일, 데이터 교환 등을 위해서 보기 좋게 데이터를 표현하는데 사용합니다.

## How to:
(어떻게 사용하나요?)

YAML 파일을 읽고 쓰려면, `gopkg.in/yaml.v3` 패키지가 필요합니다.

```Go
package main

import (
    "fmt"
    "io/ioutil"
    "gopkg.in/yaml.v3"
)

type Config struct {
    Version string `yaml:"version"`
    Services map[string]Service `yaml:"services"`
}

type Service struct {
    Image string `yaml:"image"`
    Ports []string `yaml:"ports"`
}

func main() {
    configFile, err := ioutil.ReadFile("docker-compose.yml")
    if err != nil {
        panic(err)
    }

    var config Config
    err = yaml.Unmarshal(configFile, &config)
    if err != nil {
        panic(err)
    }

    fmt.Printf("버전: %s\n", config.Version)
    for name, service := range config.Services {
        fmt.Printf("서비스: %s, 이미지: %s\n", name, service.Image)
    }
}
```

## Deep Dive
(깊이 있는 정보)

YAML은 "YAML Ain't Markup Language"(YAML은 마크업 언어가 아니다)의 재귀 약자입니다. JSON의 대안으로 보기 쉽상 외에 데이터를 간결하게 나타낼 수 있어 인기 있습니다. 그럼에도 파싱은 JSON보다 복잡하며, 대규모 데이터에는 성능 문제가 있을 수 있습니다.

## See Also
(참고 자료)

- YAML 공식 웹사이트: https://yaml.org
- go-yaml 라이브러리: https://pkg.go.dev/gopkg.in/yaml.v3
- JSON과 YAML 비교: https://en.wikipedia.org/wiki/YAML#Comparison_with_JSON
