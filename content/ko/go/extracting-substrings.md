---
title:                "부분 문자열 추출"
date:                  2024-01-20T17:46:26.607303-07:00
model:                 gpt-4-1106-preview
simple_title:         "부분 문자열 추출"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (무엇 그리고 왜?)
문자열에서 부분 문자열을 추출하는 것은 지정된 범위의 문자를 가져오는 것입니다. 프로그래머가 데이터 파싱, 검색 또는 텍스트 처리 시 특정 문자 또는 패턴에 접근해야 할 필요가 있을 때 이를 수행합니다.

## How to: (어떻게 하나요?)
Go에서 부분 문자열을 추출하는 기본적인 방법은 `slice`를 사용하는 것입니다. 문자열의 특정 부분을 가져오려면 시작 인덱스와 끝 인덱스를 정하면 됩니다.

```Go
package main

import (
    "fmt"
)

func main() {
    str := "안녕하세요. Go를 사용한 프로그래밍을 배우고 있습니다."
    
    // 문자열에서 'Go' 추출
    start := 7
    end := 9
    substring := str[start:end]
    fmt.Println(substring)  // "Go"

    // 문자열에서 '프로그래밍' 추출
    start = 14
    end = 20
    substring = str[start:end]
    fmt.Println(substring)  // "프로그래밍"
}
```
실행 결과:
```
Go
프로그래밍
```

## Deep Dive (심층적 이해)
부분 문자열을 추출하는 것은 프로그래밍의 기본적인 작업 중 하나입니다. Go에선 문자열을 불변(immutable)으로 취급하여, 문자열의 일부만 사용할 때 새로운 문자열이 생성되지 않습니다. 이는 메모리를 효율적으로 사용한다는 장점이 있습니다.

역사적 맥락으로 봤을 때, 다른 언어들에서도 슬라이싱(slicing)이나 서브스트링(substring)과 같은 기능들은 오래전부터 기본적으로 포함되어 왔습니다. Go 언어는 인덱스를 이용한 슬라이스 기능을 통해 이러한 작업을 간단하게 만들었습니다.

대체 방법으로는 `strings` 패키지 내의 함수들을 사용하는 것입니다. 예를 들어 `strings.Split()` 함수는 문자열을 분리하여 슬라이스를 만들고, `strings.Index()` 함수로 특정 문자 또는 텍스트의 시작 인덱스를 찾을 수 있습니다.

## See Also (추가 정보)
- Go 공식 문서의 문자열 처리 방법: https://golang.org/pkg/strings/
- Go 슬라이스에 대한 자세한 정보: https://blog.golang.org/slices
