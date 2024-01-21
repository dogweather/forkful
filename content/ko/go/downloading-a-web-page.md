---
title:                "웹 페이지 다운로드하기"
date:                  2024-01-20T17:44:19.917300-07:00
model:                 gpt-4-1106-preview
simple_title:         "웹 페이지 다운로드하기"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)

웹 페이지 다운로드는 인터넷 상의 문서를 로컬 시스템으로 가져오는 것입니다. 프로그래머는 자동화된 데이터 수집, 서비스 모니터링, 콘텐츠 검색 등을 위해 이 기능을 사용합니다.

## How to: (방법)

Go 언어로 웹 페이지를 다운로드하는 기본적인 방법입니다. 아래의 예제 코드와 출력 결과를 확인해보세요.

```Go
package main

import (
	"fmt"
	"io/ioutil"
	"net/http"
)

func main() {
	resp, err := http.Get("http://example.com")
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		panic(err)
	}
	fmt.Println(string(body))
}
```

실행 결과, `http://example.com` 웹 페이지의 HTML 내용을 콘솔에 출력합니다.

## Deep Dive (심층 탐구)

웹 페이지 다운로드는 초기 인터넷 시절부터 필요한 작업입니다. CURL과 같은 명령줄 도구도 유사한 일을 하지만, Go 언어는 내장된 `http` 패키지로 코딩 레벨에서 더 섬세한 제어가 가능합니다.

Go의 `http.Get` 함수는 간단한 GET 요청을 처리하는 데 사용됩니다. 오류 처리가 중요하며, `resp.Body`는 사용 후에 닫혀야 합니다(`defer` 사용). `ioutil.ReadAll`은 `resp.Body`에서 전체 본문을 읽어 옵니다.

웹 페이지 크롤링을 할 때는 정책을 준수하고, `robots.txt` 파일을 확인하는 것이 중요합니다. 대안으로는 `Colly`와 같은 Go 크롤러 라이브러리가 있습니다.

## See Also (참고 자료)

- Go http 패키지 문서: https://pkg.go.dev/net/http
- Go 웹 크롤링 라이브러리 Colly: http://go-colly.org/
- CURL 사용법 안내: https://curl.se/docs/manual.html
- robots.txt 파일에 대한 안내: https://developers.google.com/search/docs/advanced/robots/intro