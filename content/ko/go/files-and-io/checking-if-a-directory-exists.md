---
title:                "디렉토리가 존재하는지 확인하기"
aliases: - /ko/go/checking-if-a-directory-exists.md
date:                  2024-02-03T17:52:44.069708-07:00
model:                 gpt-4-0125-preview
simple_title:         "디렉토리가 존재하는지 확인하기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

Go에서 디렉토리가 있는지 확인하는 것은 파일 시스템과 상호 작용하는 애플리케이션이 디렉토리에 접근하거나 수정하려고 할 때 오류를 피하기 위해 중요합니다. 이 작업은 파일 작업의 전제 조건을 보장하거나, 구성 관리, 그리고 특정 디렉토리 구조에 의존하는 소프트웨어를 배포하는 것과 같은 작업에 필수적입니다.

## 방법:

Go에서는 `os` 패키지가 운영 체제와 상호 작용하는 기능을 제공하며, 이에는 디렉토리가 있는지 확인하는 기능도 포함됩니다. 다음은 그 방법입니다:

```go
package main

import (
    "fmt"
    "os"
)

// isDirExists는 디렉토리가 존재하는지 확인합니다
func isDirExists(path string) bool {
    info, err := os.Stat(path)
    if os.IsNotExist(err) {
        return false
    }
    return info.IsDir()
}

func main() {
    dirPath := "/tmp/exampleDir"

    if isDirExists(dirPath) {
        fmt.Printf("디렉토리 %s가 존재합니다.\n", dirPath)
    } else {
        fmt.Printf("디렉토리 %s가 존재하지 않습니다.\n", dirPath)
    }
}
```
예시 출력:

```
디렉토리 /tmp/exampleDir가 존재합니다.
```
또는 

```
디렉토리 /tmp/exampleDir가 존재하지 않습니다.
```

`/tmp/exampleDir`이 존재하는지에 따라 달라집니다.

## 깊이 있는 탐구

함수 `os.Stat`은 `FileInfo` 인터페이스와 오류를 반환합니다. 오류의 유형이 `os.ErrNotExist`라면, 디렉토리가 존재하지 않는 것을 의미합니다. 오류가 없다면, 우리는 `FileInfo` 인터페이스에서 `IsDir()` 메소드를 통해 경로가 실제로 디렉토리를 참조하는지 추가로 확인합니다.

이 방법은 단순함과 효과성으로 인해 두드러지지만, 생성 또는 쓰기와 같은 작업을 하기 전에 디렉토리 존재 여부를 확인하는 것은 동시성 환경에서 경쟁 상태를 초래할 수 있다는 점을 유의해야 합니다. 특히 동시성 애플리케이션에서는 체크하기보다는 비록 (파일 생성과 같은) 작업을 시도하고 사후에 오류를 처리하는 것이 더 안전할 수 있습니다.

역사적으로, 이러한 접근법은 직관적인 논리 때문에 프로그래밍에서 보편적이었습니다. 하지만, 멀티 스레드 및 동시성 컴퓨팅의 발전은 가능한 이러한 사전 조건 체크를 피하면서 더 견고한 오류 처리로의 전환을 필요로 합니다. 이는 조건이 덜 우려되는 단일 스레드 애플리케이션이나 스크립트에 대한 그 유용성을 감소시키지 않습니다.
