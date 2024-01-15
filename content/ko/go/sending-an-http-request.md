---
title:                "HTTP 요청 보내기"
html_title:           "Go: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 왜

HTTP 요청을 보내는 행위를 왜 할까요? 간단히 말해, HTTP 요청을 통해 웹 개발에서 필수적인 데이터를 가져오거나 전달할 수 있기 때문입니다.

## 사용 방법

HTTP 요청을 보내는 방법은 매우 간단합니다. 먼저, "net/http" 패키지를 가져온 다음 "http.Get()" 함수를 사용해 URL을 삽입하면 됩니다. 예를 들어:

```Go
package main

import (
	"fmt"
	"net/http"
)

func main() {
	resp, err := http.Get("https://example.com")
	if err != nil { // 에러 확인
		panic(err)
	}

	defer resp.Body.Close() // 이후 반드시 닫아주어야 함

	fmt.Println(resp.Status) // 응답 상태 확인

	body, err := ioutil.ReadAll(resp.Body) // 응답 바디 읽기
	if err != nil { // 에러 확인
		panic(err)
	}

	fmt.Println(string(body)) // 바디 내용 출력
}
```

위의 예제 코드를 실행하면 "200 OK"와 함께 해당 URL의 내용이 출력됩니다.

## 깊이 파고들기

실제로 HTTP 요청을 보내는 과정은 위의 예제 코드에서 간단히 보여준 것보다 훨씬 복잡합니다. HTTP 요청에서는 메서드, 헤더, 바디 등 다양한 정보를 지정해야 합니다. 또한, 에러 처리와 응답 상태 코드에 따른 다양한 처리도 필요합니다. 더 자세한 내용은 Go 공식 문서를 참고하시기 바랍니다.

## 더 읽어보기

- [Go 공식 문서 - net/http 패키지](https://golang.org/pkg/net/http/)
- [HTTP 요청 보내는 방법 (재민님 블로그)](https://blog.jemin.dev/posts/http-request-in-go/)