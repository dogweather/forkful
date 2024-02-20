---
date: 2024-01-20 17:57:00.031858-07:00
description: "\uCEE4\uB9E8\uB4DC \uB77C\uC778 \uC778\uC790\uB97C \uC77D\uB294 \uAC83\
  \uC740 \uC0AC\uC6A9\uC790\uAC00 \uD504\uB85C\uADF8\uB7A8 \uC2E4\uD589 \uC2DC \uC81C\
  \uACF5\uD558\uB294 \uC785\uB825 \uAC12\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uB2E4\uC591\uD55C \uC2DC\uB098\uB9AC\uC624\uC5D0\uC11C \uC0AC\
  \uC6A9\uC790 \uB9DE\uCDA4\uD615 \uB3D9\uC791\uC744 \uAD6C\uD604\uD558\uAE30 \uC704\
  \uD574 \uC774\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: 2024-02-19 22:05:14.663931
model: gpt-4-1106-preview
summary: "\uCEE4\uB9E8\uB4DC \uB77C\uC778 \uC778\uC790\uB97C \uC77D\uB294 \uAC83\uC740\
  \ \uC0AC\uC6A9\uC790\uAC00 \uD504\uB85C\uADF8\uB7A8 \uC2E4\uD589 \uC2DC \uC81C\uACF5\
  \uD558\uB294 \uC785\uB825 \uAC12\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uB2E4\uC591\uD55C \uC2DC\uB098\uB9AC\uC624\uC5D0\uC11C \uC0AC\uC6A9\
  \uC790 \uB9DE\uCDA4\uD615 \uB3D9\uC791\uC744 \uAD6C\uD604\uD558\uAE30 \uC704\uD574\
  \ \uC774\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uBA85\uB839\uC904 \uC778\uC218 \uC77D\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 왜 사용하나요?)
커맨드 라인 인자를 읽는 것은 사용자가 프로그램 실행 시 제공하는 입력 값입니다. 프로그래머들은 다양한 시나리오에서 사용자 맞춤형 동작을 구현하기 위해 이를 사용합니다.

## How to: (어떻게 사용하나요?)
Swift 프로그램에서 커맨드 라인 인자를 읽으려면 `CommandLine`을 사용합니다. 간단한 예제를 살펴보죠.

```Swift
// main.swift
for argument in CommandLine.arguments {
    print("Argument: \(argument)")
}
```

터미널에서 이렇게 실행하면 됩니다:
```shell
$ swift run MyProgram arg1 arg2
```

출력 예시:
```
Argument: /path/to/MyProgram
Argument: arg1
Argument: arg2
```

`CommandLine.arguments`는 배열이며, 첫 번째 아이템은 실행 파일의 경로입니다.

## Deep Dive (심화 정보)
커맨드 라인 인자를 읽는 과정은 유닉스와 같은 시스템에서 시작되었고, 매우 오래된 전통입니다. 대안으로 환경 변수(read with `ProcessInfo.processInfo.environment`) 또는 구성 파일을 사용할 수도 있습니다.

실제 구현을 보면, Swift의 `CommandLine`은 C 언어의 `argc`와 `argv`에 기반을 두고 있습니다. `CommandLine.arguments`는 `argv`를 Swift 배열로 변환한 것입니다. 첫 번째 인자가 실행 파일인 것은 `argv`의 전통을 따르는 것입니다.

## See Also (더 알아보기)
- [Swift Documentation - CommandLine](https://developer.apple.com/documentation/swift/commandline)
- [GNU - Using the Command Line](https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html)
