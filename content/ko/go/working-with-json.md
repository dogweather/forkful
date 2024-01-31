---
title:                "JSON 다루기"
date:                  2024-01-19
html_title:           "Arduino: JSON 다루기"
simple_title:         "JSON 다루기"

category:             "Go"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
JSON은 JavaScript Object Notation의 약자로, 데이터 교환 포맷입니다. 가볍고, 읽기 쉽고, 다양한 프로그래밍 언어에서 지원하기 때문에 프로그래머들이 주로 API 통신 및 설정 파일 작성에 사용합니다.

## How to: (어떻게 하나요?)
```Go
package main

import (
	"encoding/json"
	"fmt"
	"log"
)

// User 정의
type User struct {
	ID       int    `json:"id"`
	Name     string `json:"name"`
	Active   bool   `json:"active"`
}

func main() {
	// JSON 인코딩
	user := User{ID: 1, Name: "홍길동", Active: true}
	jsonData, err := json.Marshal(user)
	if err != nil {
		log.Fatalf("JSON 마샬링 에러: %v", err)
	}
	fmt.Println(string(jsonData))

	// JSON 디코딩
	var decodedUser User
	err = json.Unmarshal(jsonData, &decodedUser)
	if err != nil {
		log.Fatalf("JSON 언마샬링 에러: %v", err)
	}
	fmt.Printf("%+v\n", decodedUser)
}
```
출력:
```
{"id":1,"name":"홍길동","active":true}
{ID:1 Name:홍길동 Active:true}
```

## Deep Dive (심화 학습)
JSON의 기원은 JavaScript 언어로부터 나왔습니다만, 어떠한 프로그램 언어와도 잘 어울리는 포맷입니다. XML과 비교하면 더 가볍고, 가독성이 높습니다. Go 언어에서는 `encoding/json` 패키지를 사용하여 JSON 데이터를 처리하며, 구조체 태그를 통해 JSON 키와 구조체 필드간 맵핑을 정의 할 수 있습니다. 또한, 대안으로 `jsoniter` 같은 서드파티 라이브러리들이 존재하여 성능 향상 및 다양한 기능을 제공합니다.

## See Also (관련 자료)
- Go 공식 문서 JSON 패키지: [encoding/json](https://golang.org/pkg/encoding/json/)
- JSON 공식 웹사이트: [json.org](https://www.json.org/json-en.html)
- 고성능 JSON 라이브러리: [jsoniter](http://jsoniter.com/)
