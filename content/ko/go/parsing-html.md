---
title:                "HTML 파싱"
date:                  2024-01-20T15:32:11.778839-07:00
html_title:           "Arduino: HTML 파싱"
simple_title:         "HTML 파싱"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
HTML 파싱은 웹페이지의 구조를 분석해 데이터를 추출하는 과정입니다. 프로그래머들은 웹 콘텐츠를 자동으로 처리하거나 웹 스크래핑을 통해 정보를 수집하기 위해 이를 수행합니다.

## How to: (방법)
Go 언어에서 `net/html` 패키지를 사용하여 HTML을 파싱하는 방법을 살펴봅시다. 아래는 간단한 예제 코드입니다.

```Go
package main

import (
	"fmt"
	"golang.org/x/net/html"
	"strings"
)

func main() {
	doc, err := html.Parse(strings.NewReader("<html><body><h1>Hello, World!</h1></body></html>"))
	if err != nil {
		panic("파싱에 실패했습니다.")
	}

	var f func(*html.Node)
	f = func(n *html.Node) {
		if n.Type == html.ElementNode && n.Data == "h1" {
			fmt.Println(n.FirstChild.Data)
		}
		for c := n.FirstChild; c != nil; c = c.NextSibling {
			f(c)
		}
	}
	f(doc)
}
```

실행 결과는 아래와 같습니다.

```
Hello, World!
```

이 코드는 간단한 HTML 문자열에서 `<h1>` 태그의 텍스트를 찾아 출력합니다.

## Deep Dive (심도 있는 분석)
- **역사적 맥락**: 초기 웹 개발 시 HTML 파싱은 주로 정규 표현식을 사용했지만, 이 방법은 복잡하고 오류가 많았습니다. 브라우저가 DOM(Document Object Model)을 지원하면서 파서가 등장했습니다.
- **대안**: `net/html` 외에도, `htmlquery`, `goquery` 같은 라이브러리도 있습니다. 각각의 장단점이 있으니 상황에 맞춰 선택하면 됩니다.
- **구현 세부사항**: Go의 `net/html` 패키지는 HTML5 파싱 알고리즘을 구현합니다. 트리 구조의 DOM을 생성하고, 노드를 탐색하거나 수정하는 기능을 제공합니다. 매우 효율적이고 오류에 강합니다.

## See Also (관련 자료)
- Go 문서 [net/html package](https://pkg.go.dev/golang.org/x/net/html)
- [`htmlquery`](https://github.com/antchfx/htmlquery) 파서
- [`goquery`](https://github.com/PuerkitoBio/goquery) - jQuery 스타일의 문서 탐색 가능
