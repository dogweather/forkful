---
title:                "웹 페이지 다운로드"
html_title:           "Go: 웹 페이지 다운로드"
simple_title:         "웹 페이지 다운로드"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
웹 페이지를 다운로드한다는 것은 무엇을 의미할까요? 프로그래머들이 이 작업을 왜 수행할까요?
웹 페이지를 다운로드한다는 건 단순히 인터넷에서 웹 페이지를 내 컴퓨터로 가져오는 것을 말합니다. 프로그래머들은 이 작업을 통해 웹 페이지의 내용을 분석하고 가공할 수 있습니다.

## 방법:
원하는 웹 페이지를 다운로드하기 위해 Go 언어를 사용해보세요. 아래 코드 블록에서 다운로드한 웹 페이지의 내용을 볼 수 있습니다.

```Go
import "net/http"

res, err := http.Get("http://example.com")
if err != nil {
    panic(err)
}

defer res.Body.Close() // 다운로드 후에는 꼭 body를 닫아주어야 합니다.

body, err := ioutil.ReadAll(res.Body)
if err != nil {
    panic(err)
}

fmt.Println(string(body)) // 다운로드한 웹 페이지의 내용을 출력합니다.
```

## 더 깊이 들어가보기:
웹 페이지 다운로드는 인터넷의 초창기부터 사용되어온 방법입니다. Go 언어를 사용하지 않고도 웹 페이지를 다운로드할 수 있는 다른 방법들이 있지만, Go 언어를 사용하면 간단하고 효율적으로 다운로드할 수 있습니다. 코드의 구현 방식이나 패키지의 제공 상태 등도 꼭 살펴보세요.

## 관련 자료:
관련된 자료들을 살펴보세요.
- [Go 언어 공식 문서](https://golang.org/doc/)
- [네트워킹과 인터넷 프로그래밍에 대한 Go 언어의 공식 튜토리얼](https://golang.org/doc/articles/wiki/)