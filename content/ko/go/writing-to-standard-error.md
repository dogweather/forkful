---
title:                "Go: 표준 오류로 쓰는 방법"
simple_title:         "표준 오류로 쓰는 방법"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 왜

표준 오류에 쓰기에 참여하는 이유는 무엇일까요? Go 언어 개발자로서 가끔씩 표준 오류에 메시지를 출력해야 하는 경우가 있을 수 있습니다. 이 글에서는 그 이유와 방법에 대해 알아보겠습니다.

## 어떻게 하나요?

먼저 Go 언어에서 표준 오류에 쓰는 방법을 알아보겠습니다. 아래의 ```Go ... ``` 코드 블록에서 코드 예제와 샘플 출력을 확인할 수 있습니다.

```Go
// 기본적인 표준 오류에 쓰는 방법
fmt.Fprintln(os.Stderr, "표준 오류에 쓰는 메시지")

// 오류 핸들링과 함께 사용하는 예제
func Divide(x,y int) {
    if y == 0 {
        // 표준 오류에 에러 메시지 출력
        fmt.Fprintln(os.Stderr, "0으로 나눌 수 없습니다.")
    }
    return x / y
}
```

위의 예제에서 ```fmt.Fprintln()``` 함수를 사용하여 표준 오류에 메시지를 출력하는 방법을 볼 수 있습니다. 또한 오류 핸들링 예제에서는 함수 내부에서 오류가 발생할 경우 표준 오류에 에러 메시지를 출력하는 것을 볼 수 있습니다.

## 더 깊게 들어가기

표준 오류에 쓰는 방법에 대해 좀 더 깊게 알아보겠습니다. 표준 출력과 마찬가지로 ```os.Stderr``` 파일 디스크립터를 사용하여 오류 메시지를 출력할 수 있습니다. 또한 ```fmt.Fprintln()``` 함수 외에도 다른 함수를 사용하여 더욱 다양한 방법으로 표준 오류에 쓸 수 있습니다.

## 관련 자료

* [Go 언어 공식 문서 - 표준 오류 출력](https://golang.org/pkg/fmt/#Fprintln)
* [Go 언어 공식 문서 - 파일](https://golang.org/pkg/os/#File)
* [Go 언어 공부하기 - 표준 오류에 메시지 출력하기](https://go.dev/blog/errors-are-values)