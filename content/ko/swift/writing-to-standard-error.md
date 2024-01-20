---
title:                "표준 에러 쓰기"
html_title:           "Swift: 표준 에러 쓰기"
simple_title:         "표준 에러 쓰기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

"표준 에러에 쓰는 작성"이란 무엇인가요? 프로그래머들이 왜 이것을 할까요?
 
표준 에러에 쓰는 작성은 프로그래머들이 프로그램 실행 중에 에러가 발생할 때 에러 메세지를 출력하기 위해 사용하는 것입니다. 이를 통해 프로그램 실행 전에 발생할 수 있는 잠재적인 문제를 사전에 알 수 있게 됩니다.

## 어떻게:

 ```Swift
print("Oops! 에러 발생!", to: &stderr)
```

출력: ```"Oops! 에러 발생!"```

## 깊게 파보기:

- 위의 예시에서 볼 수 있듯이, `to:` 매개변수를 사용하여 `print()` 함수를 호출하면 표준 에러에 쓰는 작성을 할 수 있습니다.
- 다른 대안으로는 `error()` 함수를 사용하는 것이 있습니다. 이 함수는 에러 메세지를 출력하는 것 외에도 에러 코드를 지정할 수 있습니다.
- 표준 에러에 쓰는 작성은 에러 처리에 필수적인 기능이지만 너무 많이 사용하면 프로그램 실행 속도가 느려질 수 있으니 주의해야 합니다.

## 관련 자료:

- [Swift 문서 - 표준 출력과 표준 에러](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID282)
- [Swift WikiDocs - 오류 처리](https://wikidocs.net/476)