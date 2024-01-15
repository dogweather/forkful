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

## 왜
JSON은 현대의 개발 프로젝트에서 매우 중요한 역할을 합니다. 이것은 데이터를 쉽게 저장하고 전송 할 수있는 인기있는 형식입니다.

## 어떻게
JSON을 더 재미있게 다루기 위해 Go언어를 이용해봅시다. 아래에는 간단한 코딩 예제와 그 결과물이 있습니다.

```Go
// 새로운 JSON 제작
package main

import (
	"encoding/json"
	"fmt"
)

func main() {
	// 빈 JSON
	emptyJSON := make(map[string]interface{})

	// key와 value 추가
	emptyJSON["name"] = "John"
	emptyJSON["age"] = 30
	emptyJSON["hobbies"] = []string{"reading", "drawing", "coding"}

	// JSON을 string으로 변환하여 출력
	jsonString, _ := json.Marshal(emptyJSON)
	fmt.Println(string(jsonString))

	// string을 다시 JSON으로 변환하여 출력
	var newJSON map[string]interface{}
	json.Unmarshal(jsonString, &newJSON)
	fmt.Println(newJSON)
}
```

결과:

```JSON
{"name": "John", "age": 30, "hobbies": ["reading", "drawing", "coding"]}
map[name:John age:30 hobbies:[reading drawing coding]]
```

## 깊게 파헤치기
JSON을 더 자세히 살펴보면, "key-value" 쌍으로 이루어진 데이터로 이뤄져 있고, Go언어에서는 map데이터 구조를 사용하여 쉽게 다룰 수 있습니다.

그리고 Go언어에서는 "encoding/json" 패키지를 사용하여 JSON 데이터를 더 편리하게 다룰 수 있도록 해줍니다. 강력한 기능과 다양한 메서드들이 있으니 꼭 살펴보세요.

## 더 알아보기
"encoding/json" 패키지 외에도 Go언어에서 JSON을 다루는 데에 유용한 다른 패키지들이 있습니다. 자세한 내용은 아래의 링크들을 참고해주세요.

- [JSON과 Go언어 문서](https://golang.org/pkg/encoding/json/)
- [Go언어로 JSON 다루는 방법1](https://zetcode.com/golang/json/)
- [Go언어로 JSON 다루는 방법2](https://tutorialedge.net/golang/parsing-json-with-golang/)
- [JSON 예제와 설명](https://json.org/example.html)

## 참고
- 다른 개발 언어에서도 JSON을 쉽게 다룰 수 있습니다. Python에서도 더 많은 정보를 알고 싶다면, [이 글](https://realpython.com/python-json/)을 참고해주세요.
- 웹 사이트 개발에서도 JSON을 많이 사용합니다. HTML, CSS, JavaScript도 배워보는 것을 추천드립니다.