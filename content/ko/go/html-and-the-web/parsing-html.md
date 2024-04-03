---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:00:13.643883-07:00
description: "Go\uC5D0\uC11C HTML \uD30C\uC2F1\uC740 HTML \uD30C\uC77C\uC758 \uB0B4\
  \uC6A9\uC744 \uBD84\uC11D\uD558\uC5EC \uB370\uC774\uD130\uB97C \uCD94\uCD9C\uD558\
  \uAC70\uB098, \uAD6C\uC870\uB97C \uC870\uC791\uD558\uAC70\uB098, HTML\uC744 \uB2E4\
  \uB978 \uD615\uC2DD\uC73C\uB85C \uBCC0\uD658\uD558\uB294 \uAC83\uC744 \uB9D0\uD569\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC6F9 \uC2A4\uD06C\uB798\
  \uD551, \uD15C\uD50C\uB9BF\uD654 \uBC0F \uB370\uC774\uD130 \uB9C8\uC774\uB2DD\uC744\
  \ \uC704\uD574 \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD558\uBA70, Go\uC758 \uAC15\
  \uB825\uD55C \uB3D9\uC2DC\uC131 \uAE30\uB2A5\uC744 \uD65C\uC6A9\uD558\uC5EC \uB300\
  \uB7C9\uC758\u2026"
lastmod: '2024-03-13T22:44:54.451696-06:00'
model: gpt-4-0125-preview
summary: "Go\uC5D0\uC11C HTML \uD30C\uC2F1\uC740 HTML \uD30C\uC77C\uC758 \uB0B4\uC6A9\
  \uC744 \uBD84\uC11D\uD558\uC5EC \uB370\uC774\uD130\uB97C \uCD94\uCD9C\uD558\uAC70\
  \uB098, \uAD6C\uC870\uB97C \uC870\uC791\uD558\uAC70\uB098, HTML\uC744 \uB2E4\uB978\
  \ \uD615\uC2DD\uC73C\uB85C \uBCC0\uD658\uD558\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\
  \uB2E4."
title: "HTML \uD30C\uC2F1"
weight: 43
---

## 방법:
Go에서 HTML을 파싱하기 위해 일반적으로 `goquery` 패키지나 표준 라이브러리의 `net/html` 패키지를 사용합니다. 여기 웹페이지에서 모든 링크를 추출하기 위해 `net/html`을 사용한 기본 예제가 있습니다:

```go
package main

import (
    "fmt"
    "golang.org/x/net/html"
    "net/http"
)

func main() {
    // HTML 문서 가져오기
    res, err := http.Get("http://example.com")
    if err != nil {
        panic(err)
    }
    defer res.Body.Close()

    // HTML 문서 파싱
    doc, err := html.Parse(res.Body)
    if err != nil {
        panic(err)
    }

    // DOM을 재귀적으로 순회하는 함수
    var f func(*html.Node)
    f = func(n *html.Node) {
        if n.Type == html.ElementNode && n.Data == "a" {
            for _, a := range n.Attr {
                if a.Key == "href" {
                    fmt.Println(a.Val)
                    break
                }
            }
        }
        for c := n.FirstChild; c != nil; c = c.NextSibling {
            f(c)
        }
    }

    // DOM 순회
    f(doc)
}
```

샘플 출력 (가정: `http://example.com`에 두 개의 링크가 포함되어 있음):

```
http://www.iana.org/domains/example
http://www.iana.org/domains/reserved
```

이 코드는 HTML 페이지를 요청하고, 파싱하고, DOM을 재귀적으로 순회하여 모든 `<a>` 태그의 `href` 속성을 찾아서 출력합니다.

## 심층 분석
`net/html` 패키지는 HTML5 표준에 명시된 토큰화 및 트리 구축 알고리즘을 직접 구현하여 Go에서 HTML 파싱의 기본을 제공합니다. 이 저수준 접근 방식은 강력하지만 복잡한 작업에 대해 장황할 수 있습니다.

반면에, jQuery에서 영감을 받은 서드파티 `goquery` 패키지는 DOM 조작 및 순회를 간소화하는 고수준 인터페이스를 제공합니다. 개발자가 요소 선택, 속성 추출 및 콘텐츠 조작과 같은 작업에 대해 간결하고 표현력 있는 코드를 작성할 수 있도록 합니다.

하지만, `goquery`의 편리함은 추가적인 의존성과 그 추상화 레이어로 인해 발생하는 잠재적으로 느린 성능의 대가로 이루어집니다. `net/html`과 `goquery`(또는 다른 파싱 라이브러리) 사이의 선택은 프로젝트의 특정 요구 사항, 예를 들면 성능 최적화나 사용 용이성의 필요성에 따라 달라집니다.

역사적으로, Go에서의 HTML 파싱은 기본적인 문자열 연산에서 복잡한 DOM 트리 조작까지 발전하였으며, 이는 언어의 성장하는 생태계와 커뮤니티의 강력한 웹 스크래핑 및 데이터 추출 도구에 대한 요구를 반영합니다. 기본 기능에도 불구하고, `goquery`와 같은 서드파티 라이브러리의 유병함은 Go 커뮤니티가 모듈화되고 재사용 가능한 코드를 선호한다는 것을 강조합니다. 하지만, 성능에 민감한 애플리케이션의 경우, 프로그래머들은 여전히 `net/html` 패키지나 심지어는 간단한 파싱 작업을 위해 정규 표현식(regex)을 선호할 수 있으며, 정규 표현식 기반 HTML 파싱의 내재된 위험성과 한계를 염두에 두어야 합니다.
