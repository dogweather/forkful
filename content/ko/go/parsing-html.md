---
title:                "HTML 파싱"
html_title:           "Fish Shell: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/parsing-html.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

HTML 파싱은 웹 문서의 구조를 이해하고 내용을 추출하는 것입니다. 프로그래머들은 이를 통해 데이터를 수집하고, 웹사이트를 분석하며, 웹 크롤링 같은 작업을 수행합니다.

## 어떻게?

```Go
package main

import (
    "fmt"
    "golang.org/x/net/html"
    "net/http"
)

func main() {
    resp, err := http.Get("http://example.com")
    if err != nil {
        panic(err)
    }
    defer resp.Body.Close()

    z := html.NewTokenizer(resp.Body)

    for {
        tt := z.Next()
        switch {
        case tt == html.ErrorToken:
            return
        case tt == html.StartTagToken:
            t := z.Token()

            if t.Data == "p" {
                fmt.Println("We found a paragraph!")
            }
        }
    }
}
```

위의 Go 프로그램이 실행되면 "http://example.com" 웹페이지의 모든 "p" (paragraph) Tags을 찾아 'We found a paragraph!'를 출력합니다.

## 깊이 들여다보기

HTML 파싱은 웹의 태동과 거의 동시에 시작되었으며, 여러가지 방법론과 도구가 이를 지원합니다. Go 언어의 경우, 'golang.org/x/net/html' 패키지가 HTML 파싱을 도와주며, 스트림 기반의 방식을 사용하여 메모리 를 효율적으로 관리합니다. 

## 참고

- HTML 파싱 개요: https://en.wikipedia.org/wiki/HTML_parsing
- Go 라이브러리 문서: https://golang.org/pkg/net/http/
- Go net/html 패키지: https://godoc.org/golang.org/x/net/html