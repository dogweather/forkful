---
title:                "표준 오류에 쓰기"
aliases:
- ko/go/writing-to-standard-error.md
date:                  2024-02-03T18:15:34.118436-07:00
model:                 gpt-4-0125-preview
simple_title:         "표준 오류에 쓰기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇이며 왜?

Go에서 표준 오류(stderr)로 쓰기는 메인 출력 스트림이 아닌 오류 메시지나 진단을 보내는 것을 포함합니다. 프로그래머들은 이를 사용하여 정규 출력과 오류 정보를 분리함으로써 디버깅과 로그 파싱을 더욱 쉽게 만듭니다. 

## 방법:

Go에서 `os` 패키지는 표준 오류 파일을 나타내는 `Stderr` 값을 제공합니다. 이를 `fmt.Fprint`, `fmt.Fprintf`, 또는 `fmt.Fprintln` 함수와 함께 사용하여 stderr로 쓸 수 있습니다. 여기 단순한 예가 있습니다:

```go
package main

import (
    "fmt"
    "os"
)

func main() {
    // stderr에 간단한 문자열 쓰기
    _, err := fmt.Fprintln(os.Stderr, "This is an error message!")
    if err != nil {
        panic(err)
    }

    // Fprintf를 사용한 포맷된 오류 메시지
    errCount := 4
    _, err = fmt.Fprintf(os.Stderr, "Process completed with %d errors.\n", errCount)
    if err != nil {
        panic(err)
    }
}
```

stderr에 대한 샘플 출력:
```
This is an error message!
Process completed with 4 errors.
```

이 메시지들은 정규 출력(stdout)이 아닌 오류 스트림에 나타나며, 대부분의 운영 체제에서 별도로 리디렉션할 수 있습니다.

## 심화 학습

표준 오류의 개념은 정규 출력과 오류 메시지를 명확히 구분하여 데이터 처리와 관리를 더욱 효율적으로 만드는 유닉스 철학에 깊이 뿌리박고 있습니다. Go에서는 `os` 패키지를 통해 stdin, stdout, stderr 파일 디스크립터에 직접 접근을 제공함으로써 이 관례를 받아들입니다.

`os.Stderr`에 직접 쓰기는 많은 애플리케이션에 적합하지만, Go는 타임스탬프 및 더 유연한 출력 구성(예: 파일로 쓰기)과 같은 추가 기능을 제공하는 `log`와 같은 더 정교한 로깅 패키지도 제공합니다. 보다 광범위한 로깅 기능이 필요한 대규모 애플리케이션 또는 경우에는 `log` 패키지를 사용하는 것이 더 나은 대안이 될 수 있습니다. Go의 오류 처리 방식, 즉 함수에서 오류를 반환하도록 권장하는 접근 방식이 stderr에 오류 메시지를 쓰는 관행과 함께 오류 관리 및 보고의 더 세밀한 제어를 허용한다는 점도 주목할 가치가 있습니다.

본질적으로, 많은 프로그래밍 언어에서 기본적인 작업인 stderr로 쓰기는 Go의 표준 라이브러리와 디자인 원칙이 더 넓은 산업 관행과 부합하면서도 Go의 특정 디자인 정신을 충족시키는 방식으로 오류 출력을 관리하는 직관적이고 고급의 경로를 제공합니다.
