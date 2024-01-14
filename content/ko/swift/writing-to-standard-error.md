---
title:                "Swift: 표준 에러에 쓰기"
simple_title:         "표준 에러에 쓰기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜 코드 작성 시에 표준 에러에 쓰는 이유는

프로그래밍 중에 문제가 발생할 때 디버깅하는 것이 중요합니다. 표준 에러를 활용하면 디버깅할 때 유용한 정보를 얻을 수 있습니다.

## 어떻게 표준 에러에 쓰는지

아래는 실제 코드 예시와 함께 표준 에러에 쓰는 방법을 보여줍니다.

```Swift
let name = "John"

// 예외 처리를 위해 표준 에러에 메시지 출력
if name.isEmpty {
    fputs("이름을 입력하세요.", stderr)
}

// 표준 에러에 메시지 입력 후 프로그램 종료
exit(1)
```

위 코드를 실행하면 표준 에러에 "이름을 입력하세요."라는 메시지가 출력됩니다. 이를 통해 예외 상황을 탐지하고 적절한 처리를 할 수 있습니다.

## 심화 학습

표준 에러를 활용하는 더 많은 방법을 탐색해보세요. 다양한 유형의 예외를 처리할 수 있는 방법도 알아보세요.

## 관련 자료

- [Swift 공식 문서 - 표준 에러 활용](https://docs.swift.org/swift-book/LanguageGuide/ControlFlow.html#//apple_ref/doc/uid/TP40014097-CH9-ID102)
- [Swift 기본 3 - 디버깅](https://blog.naver.com/PostView.nhn?blogId=the_sun_91&logNo=221022090393&proxyReferer=https%3A%2F%2Fwww.google.com%2F)