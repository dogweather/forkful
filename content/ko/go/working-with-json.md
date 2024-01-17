---
title:                "json 작업하기"
html_title:           "Go: json 작업하기"
simple_title:         "json 작업하기"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/working-with-json.md"
---

{{< edit_this_page >}}

Why & What?

JSON은 데이터를 교환할 때 사용되는 형식입니다. 여러 다른 언어로 작성된 프로그램들 사이에 데이터를 교환하는데 이용됩니다. 프로그래머들은 JSON을 사용하는 이유는 코드를 쉽게 읽고 쓰기 위해서です.

How to:

JSON을 다루는 것은 매우 간단합니다. 가장 기본적인 형식은 중괄호로 사용하며 여러 개의 필드와 값으로 이루어져 있습니다. 각 필드는 쌍따옴표로 둘러싸여 있고 값은 숫자, 문자열, 불린 등 다양한 데이터 타입으로 표현됩니다. 예를 들어, 다음은 JSON 형식의 데이터를 Go언어로 처리하는 방법입니다.

```Go
package main

import (
    "encoding/json"
    "fmt"
)

func main() {
    // 예제로 사용할 JSON 데이터
    data := []byte(`{"name": "John", "age": 30, "location": "Seoul"}`)
    
    // 구조체 정의
    type Person struct {
        Name string `json:"name"`
        Age int `json:"age"`
        Location string `json:"location"`
    }
    
    // JSON 데이터를 구조체로 변환
    var person Person
    err := json.Unmarshal(data, &person)
    if err != nil {
        fmt.Println(err)
        return
    }
    
    // 구조체의 값 출력
    fmt.Println("Name:", person.Name)
    fmt.Println("Age:", person.Age)
    fmt.Println("Location:", person.Location)
}

// Output: 
// Name: John
// Age: 30
// Location: Seoul
```

Deep Dive:

JSON 형식은 2000년대 초부터 사용되기 시작했습니다. 이전에는 XML이나 CSV 형식이 널리 사용되었지만, JSON은 더 간결하고 가독성이 좋은 형식이기 때문에 보다 많이 사용되기 시작했습니다. JSON 외에도 XML, YAML, TOML 등 여러 다른 데이터 형식이 존재하지만, JSON은 데이터 교환에 가장 널리 사용되는 형식입니다. Go 언어는 내장되어 있는 encoding/json 패키지를 이용하여 JSON 데이터를 처리할 수 있습니다. 이 패키지는 JSON 데이터를 구조체로 매핑해주는 기능을 제공합니다.

See Also:

1. The official Go documentation for JSON processing: https://golang.org/pkg/encoding/json/
2. A tutorial on using JSON in Go: https://tutorialedge.net/golang/parsing-json-with-golang/
3. Comparison of different data formats for data exchange: https://practicaltypography.com/csv-vs-json-vs-yaml-vs-xml.html