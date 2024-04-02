---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:48.254523-07:00
description: "\uBD80\uBD84 \uBB38\uC790\uC5F4 \uCD94\uCD9C\uC740 \uBB38\uC790\uC5F4\
  \uC758 \uD2B9\uC815 \uBD80\uBD84\uC744 \uADF8\uB4E4\uC758 \uC704\uCE58\uC5D0 \uAE30\
  \uBC18\uD558\uC5EC \uAC80\uC0C9\uD558\uB294 \uACFC\uC815\uC744 \uD3EC\uD568\uD569\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC785\uB825 \uD30C\uC2F1\
  , \uD615\uC2DD \uC720\uD6A8\uC131 \uD655\uC778, \uCD9C\uB825 \uC900\uBE44 \uB4F1\
  \uACFC \uAC19\uC774 \uD14D\uC2A4\uD2B8 \uB370\uC774\uD130\uB97C \uD6A8\uC728\uC801\
  \uC73C\uB85C \uCC98\uB9AC\uD558\uAC70\uB098 \uC870\uC791\uD558\uAE30 \uC704\uD574\
  \ \uC774 \uC791\uC5C5\uC744 \uC790\uC8FC \uC218\uD589\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.437060-06:00'
model: gpt-4-0125-preview
summary: "\uBD80\uBD84 \uBB38\uC790\uC5F4 \uCD94\uCD9C\uC740 \uBB38\uC790\uC5F4\uC758\
  \ \uD2B9\uC815 \uBD80\uBD84\uC744 \uADF8\uB4E4\uC758 \uC704\uCE58\uC5D0 \uAE30\uBC18\
  \uD558\uC5EC \uAC80\uC0C9\uD558\uB294 \uACFC\uC815\uC744 \uD3EC\uD568\uD569\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC785\uB825 \uD30C\uC2F1, \uD615\
  \uC2DD \uC720\uD6A8\uC131 \uD655\uC778, \uCD9C\uB825 \uC900\uBE44 \uB4F1\uACFC \uAC19\
  \uC774 \uD14D\uC2A4\uD2B8 \uB370\uC774\uD130\uB97C \uD6A8\uC728\uC801\uC73C\uB85C\
  \ \uCC98\uB9AC\uD558\uAC70\uB098 \uC870\uC791\uD558\uAE30 \uC704\uD574 \uC774 \uC791\
  \uC5C5\uC744 \uC790\uC8FC \uC218\uD589\uD569\uB2C8\uB2E4."
title: "\uBD80\uBD84 \uBB38\uC790\uC5F4 \uCD94\uCD9C\uD558\uAE30"
weight: 6
---

## 무엇 & 왜?

부분 문자열 추출은 문자열의 특정 부분을 그들의 위치에 기반하여 검색하는 과정을 포함합니다. 프로그래머들은 입력 파싱, 형식 유효성 확인, 출력 준비 등과 같이 텍스트 데이터를 효율적으로 처리하거나 조작하기 위해 이 작업을 자주 수행합니다.

## 방법:

Go에서 `string` 타입은 읽기 전용 바이트 슬라이스입니다. 부분 문자열을 추출하기 위해 주로 `slice` 구문을 사용하며, 길이 확인을 위한 내장 함수 `len()`과 더 복잡한 작업을 위한 `strings` 패키지를 함께 사용합니다. 다음은 이를 달성하는 방법입니다:

### 기본 슬라이싱

```go
package main

import (
    "fmt"
)

func main() {
    str := "Hello, World!"
    // "World" 추출
    subStr := str[7:12]
    
    fmt.Println(subStr) // 출력: World
}
```

### `strings` 패키지 사용하기

특정 부분 문자열 이전이나 이후의 문자열을 추출하는 것과 같은 더 고급 부분 문자열 추출을 위해서는 `strings` 패키지를 사용할 수 있습니다.

```go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "name=John Doe"
    // "=" 이후의 부분 문자열 추출
    subStr := strings.SplitN(str, "=", 2)[1]
    
    fmt.Println(subStr) // 출력: John Doe
}
```

Go의 문자열이 UTF-8로 인코딩되어 있고 직접적인 바이트 슬라이스가 다중 바이트 문자를 포함하는 경우 항상 유효한 문자열이 되지 않을 수 있음을 유의하는 것이 중요합니다. 유니코드 지원을 위해서는 `range` 나 `utf8` 패키지를 고려해야 합니다.

### 유니코드 문자 처리하기

```go
package main

import (
    "fmt"
    "unicode/utf8"
)

func main() {
    str := "Hello, 世界"
    // 유니코드 문자를 고려하여 부분 문자열 찾기
    runeStr := []rune(str)
    subStr := string(runeStr[7:])
    
    fmt.Println(subStr) // 출력: 世界
}
```

## 심층 분석

Go에서 부분 문자열을 추출하는 것은 슬라이스 구문과 포괄적인 표준 라이브러리 덕분에 간단합니다. 역사적으로, 이전 프로그래밍 언어들은 이러한 텍스트 조작을 처리하기 위해 더 직접적인 함수나 메소드를 제공했습니다. 하지만, Go의 접근 방식은 불변의 문자열과 유니코드 문자를 룬을 통해 명확하게 처리하는 것에 있어 안전성과 효율성을 강조합니다.

간단한 슬라이싱은 성능 효율성에서 이점을 얻지만, 직접적으로 UTF-8 문자를 다루는 복잡성을 상속받습니다. `rune` 타입의 도입은 Go 프로그램이 유니코드 텍스트를 안전하게 처리할 수 있게 하여, 국제적인 애플리케이션을 위한 강력한 대안을 제공합니다.

또한, 다른 언어에서 오는 프로그래머들은 내장된 고수준 문자열 조작 함수가 없다는 것을 느낄 수 있습니다. 그러나, Go의 표준 라이브러리에 있는 `strings`와 `bytes` 패키지는 조금 더 많은 보일러플레이트를 필요로 하지만, 부분 문자열 추출을 포함한 문자열 처리를 위한 강력한 옵션을 제공하는 풍부한 함수 세트를 제공합니다.

본질적으로, Go의 문자열 다루기 주변의 설계 선택은 현대적이고 국제화된 텍스트 데이터를 처리할 때의 단순성, 성능, 그리고 안전성에 대한 그 목표를 반영합니다. 약간의 조정이 필요할 수 있지만, Go는 부분 문자열 추출과 그 이상을 처리하기 위한 효과적이고 효율적인 도구를 제공합니다.
