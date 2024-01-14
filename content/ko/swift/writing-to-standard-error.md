---
title:                "Swift: 표준 에러에 쓰는 방법"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜
스탠더드 에러를 기록하는 것이 왜 중요한지에 대해 말씀드리겠습니다. 스위프트 프로그래밍에서 스탠더드 에러는 디버깅 및 예외 처리에 매우 유용한 정보를 제공하기 때문에 중요합니다.

## 방법
스탠더드 에러에 정보를 쓰는 방법은 간단합니다. 우선 `FileHandle.standardError` 메서드를 사용하여 스크립트에서 에러 핸들을 가져옵니다. 그런 다음 이 핸들을 사용하여 `FileHandle.write` 메서드를 호출하여 스탠더드 에러에 원하는 메시지를 기록합니다.

```Swift
let errorHandle = FileHandle.standardError
errorHandle.write("에러 발생!")
```

위의 예시 코드에서는 스탠더드 에러에 문자열 "에러 발생!"을 쓰는 방법을 보여줍니다. 여러분은 어떤 종류의 메시지든 원하는대로 스탠더드 에러에 기록할 수 있습니다.

## 깊이 파고들기
스탠더드 에러를 기록하는 것이 중요한 이유는 디버깅 및 예외 처리를 위해 필요한 정보가 스탠더드 에러에 기록되기 때문입니다. 여러분은 코드에서 예외가 발생할 때 스탠더드 에러에 해당 예외 정보를 기록하여 디버깅을 도와줄 수 있습니다. 또한 사용자에게 잘못된 입력 등의 오류를 알리는데에도 사용할 수 있습니다.

## 더 알아보기
이 글에서는 스탠더드 에러에 대해서 간략하게 살펴보았지만 실제로는 더 다양한 방법으로 사용할 수 있습니다. 여러분은 스탠더드 에러에 대해 더 알아보기 위해 다음 링크들을 참고하시길 바랍니다.

[스위프트 문서 - FileHandle](https://developer.apple.com/documentation/foundation/filehandle)  
[Swift Language Guide - Error Handling](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)

## 관련 링크
- [스위프트 문서 - FileHandle](https://developer.apple.com/documentation/foundation/filehandle)
- [Swift Language Guide - Error Handling](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
- [스탠더드 에러 사용 예시 코드](https://www.tutorialspoint.com/write-data-to-standard-error-in-swift-program)