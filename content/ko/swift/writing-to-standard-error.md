---
title:    "Swift: 표준 오류에 작성하기"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 왜 
표준 오류에 써야하는 이유는 시스템 디버깅에서 특정 프로그램의 문제를 찾는 데 도움이 되는 경로입니다.

## 사용 방법 
표준 오류를 쓰는 방법을 살펴보겠습니다. 먼저, 코드 내에서 `write(_:)` 함수를 사용하여 표준 오류에 메시지를 쓸 수 있습니다. 예를 들어:

```Swift
print("표준 오류에 메시지를 쓰는 간단한 예제입니다.", to: &.standardError)
```

그리고 이 코드를 실행하면 표준 오류에 "표준 오류에 메시지를 쓰는 간단한 예제입니다."라는 메시지가 기록됩니다. 이를 통해 프로그램 내부에서 발생한 에러를 추적하거나 다른 개발자와 협업할 때 디버깅을 도와줍니다.

## 깊이 파고들기 
표준 오류에 대한 깊은 이해를 위해, 스위프트에서 에러 핸들링을 우회하는 다른 방법을 알아보겠습니다. `write(_:)` 함수의 파라미터로 넘겨줄 수 있는 `FileDescriptor` 타입을 활용하여 오류를 쓸 수 있습니다. 예를 들어:

```Swift
let fileDescriptor = FileDescriptor.standardError
let message = "에러 메시지"
let bytesWritten = try? write(fileDescriptor.fileDescriptor, message: message)
```

또 다른 방법으로, `write(_:)` 함수 대신 `FileHandle`을 사용하여 표준 오류에 메시지를 쓸 수도 있습니다. 이 방법의 장점은 `FileHandle` 객체를 닫을 수 있다는 것입니다. 예를 들어:

```Swift
let fileHandle = FileHandle.standardError
let message = "에러 메시지"
fileHandle.write(message.data(using: .utf8)!)
fileHandle.closeFile()
```

## 같이 보기 
- [스위프트 공식 문서 - Standard Error](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html#ID522)
- [메인스웨프트 - 스위프트 표준 오류 처리](https://main.swift.or.kr/docs/swift-error-handling#%ED%91%9C%EC%A4%80-%EC%98%A4%EB%A5%98-%EC%B2%98%EB%A6%AC)