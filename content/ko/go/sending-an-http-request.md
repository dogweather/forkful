---
title:                "HTTP 요청 보내기"
html_title:           "Clojure: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 무엇이고 왜요?
HTTP 요청을 보내는 것은 웹 서버와 통신하기 위한 방법입니다. 프로그래머들은 웹 서비스와 데이터를 송수신하거나 API와 상호 작용하기 위해 이를 사용합니다.

## 어떻게 하나요:
Go에서는 `net/http` 패키지를 사용하여 HTTP 요청을 쉽게 보낼 수 있습니다. 아래는 간단한 예시입니다:

```Go
package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
)

func main() {
	resp, err := http.Get("http://example.com")
	if err != nil {
		log.Fatal(err)
	}
	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println(string(body))
}
```
이 코드를 실행하면 "http://example.com"의 HTML 내용을 출력합니다.

## 깊이 들어가보기:
HTTP 요청은 웹 프로토콜이 메인스트림이 되기 전의 1990년대부터 사용되었습니다. Go는 HTTP 요청을 손쉽게 처리할 수 있도록 `net/http` 패키지를 제공합니다. 이 외에도 `net/url` 패키지를 사용해 URL을 파싱하거나, `http.Client` 를 사용해 요청을 사용자 정의 할수도 있습니다. 

## 또 보기:
- Go Documentation: [net/http](https://golang.org/pkg/net/http/) 패키지
- Go by Example: [HTTP 클라이언트](https://gobyexample.com/http-clients) 제작
- Go Blog: [Go에서 http 요청 생성하기](https://blog.golang.org/making-http-requests-in-go)