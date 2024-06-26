---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:08:56.412887-07:00
description: "\uBC29\uBC95: Go\uC5D0\uC11C HTTP \uC694\uCCAD\uC744 \uBCF4\uB0B4\uACE0\
  \ \uC751\uB2F5\uC744 \uCC98\uB9AC\uD558\uB294 \uAC83\uC740 `net/http` \uD328\uD0A4\
  \uC9C0\uB97C \uC0AC\uC6A9\uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4\
  . \uC544\uB798\uB294 \uAC04\uB2E8\uD55C GET \uC694\uCCAD\uC744 \uBCF4\uB0B4\uACE0\
  \ \uC751\uB2F5\uC744 \uC77D\uB294 \uB2E8\uACC4\uBCC4 \uC608\uC2DC\uC785\uB2C8\uB2E4\
  ."
lastmod: '2024-03-13T22:44:54.450074-06:00'
model: gpt-4-0125-preview
summary: "Go\uC5D0\uC11C HTTP \uC694\uCCAD\uC744 \uBCF4\uB0B4\uACE0 \uC751\uB2F5\uC744\
  \ \uCC98\uB9AC\uD558\uB294 \uAC83\uC740 `net/http` \uD328\uD0A4\uC9C0\uB97C \uC0AC\
  \uC6A9\uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4."
title: "HTTP \uC694\uCCAD \uBCF4\uB0B4\uAE30"
weight: 44
---

## 방법:
Go에서 HTTP 요청을 보내고 응답을 처리하는 것은 `net/http` 패키지를 사용하는 것을 포함합니다. 아래는 간단한 GET 요청을 보내고 응답을 읽는 단계별 예시입니다:

```go
package main

import (
    "fmt"
    "io/ioutil"
    "log"
    "net/http"
)

func main() {
    // 리소스의 URL 정의하기
    url := "http://example.com"

    // http.Get을 사용해서 GET 요청 보내기
    resp, err := http.Get(url)
    if err != nil {
        log.Fatal(err)
    }
    // 함수가 끝날 때 응답 본문을 닫기
    defer resp.Body.Close()

    // 응답 본문 읽기
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        log.Fatal(err)
    }

    // 응답 본문을 문자열로 변환해서 출력하기
    fmt.Println(string(body))
}
```

예시 출력 (간략화됨):
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

양식 데이터와 함께 POST 요청을 보내려면 `http.PostForm`을 사용할 수 있습니다:

```go
package main

import (
    "fmt"
    "io/ioutil"
    "net/http"
    "net/url"
)

func main() {
    // URL과 양식 데이터 정의하기
    url := "http://example.com/form"
    data := url.Values{}
    data.Set("key", "value")

    // 양식 데이터와 함께 POST 요청 보내기
    resp, err := http.PostForm(url, data)
    if err != nil {
        panic(err)
    }
    defer resp.Body.Close()

    // 응답 읽고 출력하기
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        panic(err)
    }

    fmt.Println(string(body))
}
```

## 심층 탐구
Go의 `net/http` 패키지는 HTTP 서버와 상호작용하는 강력하고 유연한 방법을 제공합니다. 이것의 디자인은 Go의 단순성, 효율성 및 견고함에 대한 강조를 반영합니다. 원래 JSON 또는 XML 페이로드 같은 기능을 처리하기 위해서는 요청 본문을 수동으로 작성하고 적절한 헤더를 설정해야 했습니다. Go가 진화함에 따라, 커뮤니티는 `gorilla/mux` 같은 라우팅과 `gjson` 같은 JSON 조작을 더 간편하게 만드는 고수준 패키지를 개발했습니다.

Go의 HTTP 클라이언트의 주목할만한 측면 중 하나는 `http.Client`와 `http.Request`와 같은 인터페이스와 구조체의 사용입니다. 이것은 광범위한 맞춤 설정 및 테스팅을 가능하게 합니다. 예를 들어, 성능을 위해 요청을 시간 초과하게 하거나 연결을 유지하는 것과 같이 `http.Client`를 수정할 수 있습니다.

간단한 HTTP 상호작용을 위한 고려할 대안으로는 "Resty"나 "Gentleman"과 같은 타사 라이브러리의 사용이 있습니다. 이러한 패키지는 HTTP 요청에 대한 고급 추상화를 제공하여, 보다 간결하게 일반적인 작업을 수행할 수 있습니다. 그러나, 보다 복잡하거나 독특한 HTTP 상호작용 시나리오를 다루기 위해서는 기본 `net/http` 패키지를 이해하고 활용하는 것이 중요하며, Go의 동시성 기능과 강력한 표준 라이브러리를 전적으로 활용할 수 있는 기반을 제공합니다.
