---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:26.528930-07:00
description: "\uBC29\uBC95: Go\uC5D0\uC11C XML\uC744 \uD30C\uC2F1\uD558\uB824\uBA74\
  \ `encoding/xml` \uD328\uD0A4\uC9C0\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4. \uC774\
  \ \uD328\uD0A4\uC9C0\uB294 XML\uC744 Go \uAD6C\uC870\uCCB4\uB85C \uC5B8\uB9C8\uC0EC\
  \uB9C1(\uD30C\uC2F1)\uD560 \uD544\uC694\uD55C \uB3C4\uAD6C\uB97C \uC81C\uACF5\uD569\
  \uB2C8\uB2E4. \uC608\uB97C \uB4E4\uC5B4, \uB2E4\uC74C\uACFC \uAC19\uC774 \uCC45\uC744\
  \ \uB098\uD0C0\uB0B4\uB294 XML \uB370\uC774\uD130\uB97C \uACE0\uB824\uD574 \uBCF4\
  \uC138\uC694."
lastmod: '2024-03-13T22:44:54.498978-06:00'
model: gpt-4-0125-preview
summary: "Go\uC5D0\uC11C XML\uC744 \uD30C\uC2F1\uD558\uB824\uBA74 `encoding/xml` \uD328\
  \uD0A4\uC9C0\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "XML\uB85C \uC791\uC5C5\uD558\uAE30"
weight: 40
---

## 방법:


### Go에서 XML 파싱하기
Go에서 XML을 파싱하려면 `encoding/xml` 패키지를 사용합니다. 이 패키지는 XML을 Go 구조체로 언마샬링(파싱)할 필요한 도구를 제공합니다. 예를 들어, 다음과 같이 책을 나타내는 XML 데이터를 고려해 보세요:

```xml
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
```

이를 파싱하기 위해서 XML 구조를 반영하는 구조체를 정의합니다:

```go
package main

import (
    "encoding/xml"
    "fmt"
    "os"
)

type Book struct {
    XMLName xml.Name `xml:"book"`
    ID      string   `xml:"id,attr"`
    Title   string   `xml:"title"`
    Author  string   `xml:"author"`
    Pages   int      `xml:"pages"`
}

func main() {
    data := []byte(`
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
`)

    var book Book
    err := xml.Unmarshal(data, &book)
    if err != nil {
        panic(err)
    }

    fmt.Printf("Book: %+v\n", book)
}
```

출력:

```
Book: {XMLName:{Space: Local:book} ID:123 Title:Learning Go Author:John Doe Pages:359}
```

### Go에서 XML 생성하기
Go 데이터 구조에서 XML 문서를 생성하려면, 다시 `encoding/xml` 패키지를 사용합니다. 이번에는 Go 구조체를 XML로 마샬링합니다. 이전 `Book` 구조체를 가정해 보세요:

```go
package main

import (
    "encoding/xml"
    "fmt"
    "os"
)

func main() {
    book := &Book{
        ID:     "123",
        Title:  "Learning Go",
        Author: "John Doe",
        Pages:  359,
    }

    output, err := xml.MarshalIndent(book, "", "    ")
    if err != nil {
        panic(err)
    }

    fmt.Println(xml.Header + string(output))
}
```

출력:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
```

## 심층 분석
XML의 번잡함과 복잡성으로 인해 많은 응용 프로그램에서 JSON 및 기타 형식이 더욱 인기를 얻고 있습니다. 하지만, XML이 복잡한 계층적 데이터를 표현할 수 있고, 레거시 시스템 및 특정 도메인(예: SOAP 서비스)에서 광범위하게 사용되는 능력은 그 중요성을 보장합니다.

Go의 `encoding/xml` 패키지는 XML 작업을 위한 강력한 메커니즘을 제공하지만, 그 한계에 유의해야 합니다. 예를 들어, XML 네임스페이스를 처리하는 것은 번거로울 수 있으며, 보다 단순한 사용 사례에 비해 XML 사양에 대한 더 상세한 이해가 필요할 수 있습니다. 또한, Go의 정적 타이핑과 `encoding/xml` 패키지의 마샬링 및 언마샬링 기능은 일반적으로 효율적이지만, 개발자는 깊게 중첩된 구조를 다루거나 Go의 타입 시스템에 잘 매핑되지 않는 XML 문서를 다룰 때 도전에 직면할 수 있습니다.

대부분의 현대 응용 프로그램에서는 JSON과 같은 대안이 더 간단하고 효율적입니다. 하지만, XML이 필요한 상황—레거시 시스템, 특정 산업 표준, 또는 복잡한 데이터 표현 요구 사항으로 인해—에서 작업할 때, Go의 표준 라이브러리는 작업을 완수하기 위한 강력한 도구를 제공합니다. 언제나 그렇듯이, 데이터 형식의 최선의 선택은 애플리케이션과 환경의 구체적 요구 사항에 따라 달라집니다.
