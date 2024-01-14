---
title:                "Go: json 작업하기"
simple_title:         "json 작업하기"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/working-with-json.md"
---

{{< edit_this_page >}}

## 왜

JSON은 다양한 형식의 데이터를 한 곳에서 효율적으로 관리할 수 있는 가장 인기 있는 방법 중 하나입니다.

## 어떻게

Go 프로그래밍에서 JSON 데이터를 처리하는 방법은 매우 간단합니다. 먼저, encoding/json 패키지를 임포트합니다. 그런 다음, 구조체를 정의하고 필드에 태그를 추가하여 JSON 키와 필드를 연결합니다. 마지막으로, Marshal 함수를 사용하여 Go 구조체를 JSON 형식으로 변환하거나, Unmarshal 함수를 사용하여 JSON 데이터를 Go 구조체로 변환할 수 있습니다.

```Go
// 구조체 정의
type Person struct {
	Name string `json:"name"`
	Age  int    `json:"age"`
}

// Go 구조체를 JSON 형식으로 변환
person := Person{Name: "Amy", Age: 28}
jsonBytes, _ := json.Marshal(person)
fmt.Println(string(jsonBytes))
// Output: {"name":"Amy","age":28}

// JSON 데이터를 Go 구조체로 변환
jsonData := []byte(`{"name":"Bob","age":35}`)
var otherPerson Person
json.Unmarshal(jsonData, &otherPerson)
fmt.Println(otherPerson)
// Output: {Bob 35}
```

## 딥 다이브

encoding/json 패키지는 다양한 기능을 제공하여 개발자가 더욱 효율적으로 JSON 데이터를 처리할 수 있도록 도와줍니다. 예를 들어, 태그를 설정하지 않고 구조체 필드의 이름과 JSON 키가 동일한 경우, Unmarshal 함수는 자동으로 필드와 JSON 키를 대응시켜 데이터를 매핑할 수 있습니다. 또한, 특정 필드를 무시하고 싶은 경우에는 해당 필드의 태그를 "-"로 설정하면 됩니다.

## 더 알아보기

[Go 언어 공식 문서 - encoding/json 패키지](https://golang.org/pkg/encoding/json/)  
[JSON 데이터 테스트 및 디버그 툴 - JSON Formatter](https://jsonformatter.org/)  
[Go 프로그래밍에 대한 상세한 정보 - Effective Go](https://golang.org/doc/effective_go.html)

## 이어서 보기