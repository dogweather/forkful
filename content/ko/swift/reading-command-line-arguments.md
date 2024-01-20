---
title:                "명령줄 인수 읽기"
html_title:           "Arduino: 명령줄 인수 읽기"
simple_title:         "명령줄 인수 읽기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜 & 왜?
명령행 인수를 읽는다는 것은, 프로그램이 OS로부터 명령행에 입력된 인수를 읽는 것입니다. 왜냐하면 이것은 사용자가 실행 시에 프로그램에 정보를 전달하게 해주기 때문입니다.

## 이렇게 해보세요:
Swift에서 명령행 인수를 읽는 가장 간단한 방법은 `CommandLine.arguments` 배열을 사용하는 것입니다. 

```
Swift
let arguments = CommandLine.arguments
print("이 프로그램은 다음의 인수와 함께 실행되었습니다.: \(arguments)")
```

이 코드를 실행하면 이렇게 출력됩니다:

```
$ swift my_program.swift 첫번째_인수 두번째_인수
이 프로그램은 다음의 인수와 함께 실행되었습니다.: ["my_program.swift", "첫번째_인수", "두번째_인수"]
```

## 깊게 살펴보기
명령행 인수에 대해 더 깊게 알아보기 전에, 이것의 역사적 맥락을 이해하는 것이 중요합니다. 이글은 Unix에서 시작해 C언어로부터 물려받았습니다. 

또한 Swift에는 명령행 인수 외에도 다양한 사용자 입력 방법이 있습니다. 예를 들어, 표준 입력(`stdin`)을 읽는 것은 `readLine()` 함수를 사용하여 가능합니다.

대안으로, ArgumentParser 라이브러리 사용을 고려할 수 있습니다. 이 라이브러리는 POSIX와 GNU 스타일의 명령줄 인자를 파싱하고 검증하는 훌륭하고 유연한 방법을 제공합니다.

구현 세부사항을 이해하는 것도 중요합니다. `CommandLine.arguments`는 문자열 배열이며, 각 인수는 공백으로 분리된 문자열로 처리됩니다. 따라서 공백이 포함된 인수는 따옴표(" ")로 묶어야 합니다.

## 참고 자료
- [공식 Swift 문서](https://developer.apple.com/documentation/swift/commandline)
- [ArgumentParser 라이브러리](https://github.com/apple/swift-argument-parser)