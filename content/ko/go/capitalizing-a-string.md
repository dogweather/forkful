---
title:                "Go: 그것은 문자열 대문자로 만들기라는 기사 제목입니다."
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열을 대문자로 바꾸는 것에 대해 나는 왜 말할까요? 이것은 기초적이면서도 유용한 프로그래밍 스킬입니다. 예를 들어, 사용자로부터 받은 입력 값을 대문자로 바꾸어서 데이터의 일관성을 유지할 수 있습니다. 또는 출력 결과를 보기 좋게 만들기 위해서도 사용할 수 있습니다.

## 하는 방법

이제 실제 코드 예제를 살펴보겠습니다. Go 언어에서 문자열을 대문자로 바꾸기 위해서는 `strings` 패키지의 `ToUpper` 메서드를 사용합니다.

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "hello, world!"
    fmt.Println(strings.ToUpper(str))
}
```

위 코드를 실행하면 `HELLO, WORLD!`가 출력됩니다.

## 깊게 파고들기

하지만 문자열의 길이가 길어지면 매번 `strings.ToUpper`를 사용하는 것은 번거롭습니다. 이럴 때는 `bytes` 패키지를 사용해보세요. `bytes` 패키지는 문자열을 바이트 단위로 쪼개서 처리할 수 있는 기능을 제공합니다. 따라서 `ToUpper`와 같이 모든 문자를 대문자로 바꾸는 작업보다는 빠른 처리가 가능합니다.

```Go
package main

import (
    "bytes"
    "fmt"
)

func main() {
    str := "hello, world!"
    upperByte := bytes.ToUpper([]byte(str))
    fmt.Println(string(upperByte))
}
```

`ToUpper`를 사용한 코드보다 더 빠르게 실행될 것입니다. 하지만 문자열 중간에 대문자가 들어가 있는 경우에는 주의해야 합니다.

## 참고 사항

* [Go 공식 문서 - strings 패키지](https://golang.org/pkg/strings/)
* [Go 공식 문서 - bytes 패키지](https://golang.org/pkg/bytes/)
* [Go 언어로 문자열 조작하기 (Korean)](https://codingbot.net/golang/202)

## 관련 링크

* [Go 언어로 문자열을 대문자로 바꾸는 방법 (Korean)](https://streamkong.tistory.com/entry/Go-%EC%9E%85%EB%A0%A5-%EB%B0%9B%EC%9D%80-%EB%AC%B8%EC%9E%90%EC%97%90%EC%84%9C-%EB%8C%80%EB%AC%B8%EC%9E%90-%EC%B6%94%EB%9D%BC-%EB%B0%8F-%EB%AA%A8%EA%B8%B0%ED%99%94%ED%95%98%EA%B8%B0)