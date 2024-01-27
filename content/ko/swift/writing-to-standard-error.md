---
title:                "표준 오류로 쓰기"
date:                  2024-01-19
html_title:           "Bash: 표준 오류로 쓰기"
simple_title:         "표준 오류로 쓰기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
(무엇이며 왜 사용하는가?)
표준 에러는 오류 메시지를 콘솔에 출력하는 스트림입니다. 프로그래머들은 오류를 표준 출력과 분리하여 로깅과 디버깅을 용이하게 하기 위해 사용합니다.

## How to:
(어떻게 하나:)
Swift에서 표준 에러로 쓰려면 `FileHandle.standardError`를 사용합니다. 예제를 보세요:

```Swift
import Foundation

// 에러 메세지를 표준 에러에 작성하는 함수
func writeError(message: String) {
    if let data = "\(message)\n".data(using: .utf8) {
        FileHandle.standardError.write(data)
    }
}

// 사용 예
writeError(message: "허용되지 않는 값입니다.")
```

예상 출력 결과:
```
허용되지 않는 값입니다.
```
출력은 콘솔의 표준 에러에 나타납니다.

## Deep Dive:
(깊이 알아보기:)
표준 에러(stream stderr)는 과거 유닉스 시스템에서 시작되어, 표준 출력(stdout)과 별개로 오류를 관리하기 위한 방법으로 자리잡았습니다. 표준 출력(stdout) 대신 표준 에러(stderr)를 사용하면, 사용자에게 중요한 정보만 보여줄 수 있습니다. Swift에서는 `FileHandle.standardError`를 통해서 stderr에 접근할 수 있고, 오류 로그를 파일로 리다이렉션할 수 있습니다.

## See Also:
(관련 자료:)
1. Swift Documentation: https://developer.apple.com/documentation/swift
2. FileHandle API Reference: https://developer.apple.com/documentation/foundation/filehandle
3. Unix stderr Documentation: https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html
