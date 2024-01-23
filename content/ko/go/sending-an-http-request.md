---
title:                "HTTP 요청 보내기"
date:                  2024-01-20T18:00:19.782731-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP 요청 보내기"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
HTTP 요청을 보낸다는 것은 인터넷을 통해 서버에 정보를 요청하거나 보내는 행위입니다. 프로그래머들은 데이터를 가져오거나 웹 서비스와 상호 작용하기 위해 이를 수행합니다.

## How to (방법)
Go에서 HTTP 요청을 보내는 방법은 `net/http` 패키지를 사용하는 것입니다. 가장 기초적인 GET 요청부터 정리해보죠.

```go
package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
)

func main() {
	response, err := http.Get("http://example.com")
	if err != nil {
		log.Fatal(err)
	}
	defer response.Body.Close()

	body, err := ioutil.ReadAll(response.Body)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println(string(body))
}
```

실행 결과, `http://example.com`의 HTML 내용을 출력합니다.

POST 요청은 조금 다릅니다. 다음 코드를 참조하세요.

```go
package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
)

func main() {
	jsonData := []byte(`{"key1": "value1", "key2": "value2"}`)
	response, err := http.Post("http://example.com/post", "application/json", bytes.NewBuffer(jsonData))
	if err != nil {
		log.Fatal(err)
	}
	defer response.Body.Close()

	body, err := ioutil.ReadAll(response.Body)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println(string(body))
}
```

`http://example.com/post` 주소에 JSON 데이터를 보내고, 서버의 응답을 출력합니다.

## Deep Dive (심화 학습)
HTTP 요청을 보내는 과정은 원래 웹 브라우저가 담당하는 일이었습니다. 그러나, API가 흔해지면서 서버와 서버 간, 또는 클라이언트 애플리케이션과 서버 간의 통신이 필수적이 되었습니다.

Go의 `net/http` 패키지는 이런 요구를 충족하기 위해 설계되었습니다. RESTful API 통신에 적합하며, 사용하기 쉬운 인터페이스를 제공합니다.

대안으로는 `curl` 커맨드 라인 도구나 다른 프로그래밍 언어의 라이브러리가 있습니다. Go에서 특히 유용한 것은, goroutines과 채널을 사용하여 비동기적으로 HTTP 요청을 처리할 수 있다는 점입니다. 이로 인해 대규모의 동시 요청도 효율적으로 관리할 수 있습니다.

요청에 따라 `http.NewRequest` 함수를 사용하여 더 세밀한 설정의 요청 객체를 생성할 수도 있습니다. 이를 통해 헤더 설정, 쿼리 파라미터 추가, 특정 HTTP 메소드 지정 등이 가능해집니다.

## See Also (참고 자료)
- Go net/http 패키지 문서: https://golang.org/pkg/net/http/
- RESTful API 설계 가이드: https://restfulapi.net/
- Go by Example: HTTP Clients: https://gobyexample.com/http-clients
