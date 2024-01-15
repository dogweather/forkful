---
title:                "HTML 구문 분석"
html_title:           "Go: HTML 구문 분석"
simple_title:         "HTML 구문 분석"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/parsing-html.md"
---

{{< edit_this_page >}}

## 왜
산업에서 유동적인 환경의 웹 페이지를 파싱하기 위한 최근 기술인 HTML 파싱에 대해 알아보고 싶다면 이 글을 참고하세요! 이 기술은 스크래핑, 웹 크롤링 등 다양한 분야에서 사용될 수 있습니다.

## 어떻게
우리는 Go 언어를 사용하여 HTML을 파싱하는 방법을 살펴볼 것입니다. 먼저, 필요한 라이브러리를 임포트합니다.
```Go
import (
	"fmt"
	"strings"

	"golang.org/x/net/html"
)
```
그리고 우리는 다음과 같은 코드를 사용하여 HTML을 파싱합니다.
```Go
// HTML 문자열
const htmlString = "<html><body><h1>Hello, World!</h1></body></html>"

// HTML 파싱
doc, err := html.Parse(strings.NewReader(htmlString))

if err != nil {
	fmt.Println("파싱 에러:", err)
}

// 첫 번째 h1 태그를 찾습니다.
h1 := findElement(doc, "h1")

// h1 태그 안의 텍스트를 출력합니다.
fmt.Println("결과:", h1.FirstChild.Data)
// 결과: Hello, World!
```

## 깊게 들어가기
HTML 파싱에 대해 더 자세히 알아보겠습니다. HTML은 트리 구조로 이루어져 있기 때문에 우리는 트리에서 원하는 요소를 찾을 수 있습니다. 위의 예제에서 우리는 `findElement` 함수를 사용하여 첫 번째 h1 태그를 찾았는데, 이 함수는 다음과 같이 정의될 수 있습니다.
```Go
// 원하는 요소를 찾는 함수
func findElement(n *html.Node, name string) *html.Node {
	if n.Type == html.ElementNode && n.Data == name {
		return n
	}
	for c := n.FirstChild; c != nil; c = c.NextSibling {
		if result := findElement(c, name); result != nil {
			return result
		}
	}
	return nil
}
```
또 다른 예제로, 우리가 파싱하고자 하는 HTML에 여러 개의 태그가 있다고 가정해봅시다. 이 경우, `findElement` 함수를 다음과 같이 수정하여 모든 해당 태그를 찾을 수 있습니다.
```Go
// 모든 해당 태그를 찾는 함수
func findAllElements(n *html.Node, name string) []*html.Node {
	var result []*html.Node
	if n.Type == html.ElementNode && n.Data == name {
		result = append(result, n)
	}
	for c := n.FirstChild; c != nil; c = c.NextSibling {
		result = append(result, findAllElements(c, name)...)
	}
	return result
}
```

이 방법 외에도 Go 언어에는 다양한 라이브러리와 패키지가 있어서 HTML 파싱을 더 간편하게 할 수 있습니다. 또한, CSS 선택자와 같은 기능을 제공하는 라이브러리도 있어서 더욱 높은 유연성을 가질 수 있습니다.

## See Also
- [Golang.org - HTML Package](https://golang.org/pkg/net/html/)
- [Scraping HTML with Go](https://www.devdungeon.com/content/scraping-html-go)
- [Parsing HTML with Go](https://blog.golang.org/pipelines)