---
title:                "테스트 쓰기"
html_title:           "Haskell: 테스트 쓰기"
simple_title:         "테스트 쓰기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/writing-tests.md"
---

{{< edit_this_page >}}

# 왜

왜 누군가가 테스트를 작성하는 것에 참여하는지 설명하기는 어렵지 않습니다. 테스트를 작성하는 것은 코드를 더욱 견고하고 안정적이게 만들기 때문입니다. 또한 테스트는 코드를 작성하는 과정에서 자신의 작업을 확인하고 오류를 미리 발견할 수 있도록 도와 줍니다.

# 방법

```Haskell
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = putStrLn "Hello World!"
```

위의 예시는 테스트를 작성하기 위해 Hspec와 QuickCheck 라이브러리를 사용하는 방법을 보여줍니다. 먼저 ```main``` 함수를 정의하고 그 안에 ```putStrLn``` 함수를 이용해 "Hello World!" 메시지를 출력합니다. 그리고 이렇게 작성된 테스트는 ```main``` 함수에서 해당 메시지를 출력하는지 확인할 수 있습니다.

## Deep Dive

테스트를 작성하는 것은 간단한 작업처럼 보이지만 실제론 코드를 작성하는 것보다 훨씬 더 까다로운 작업입니다. 테스트는 코드의 모든 측면을 다루기 때문에 코드를 더욱 꼼꼼하게 작성해야 합니다. 또한 테스트를 작성하면 코드의 문제를 미리 찾아내기 때문에 디버깅 시간을 줄일 수도 있습니다. 따라서 테스트를 작성하는 것은 코드의 품질을 높이는 중요한 요소입니다.

## 관련 링크

- [Hspec 공식 홈페이지](https://hspec.github.io/)
- [QuickCheck 공식 홈페이지](https://hackage.haskell.org/package/QuickCheck)
- [Haskell 위키백과](https://ko.wikipedia.org/wiki/%ED%95%B8%EC%8A%A4%EC%BC%80%EB%A5%BC)

# 관련 자료

- [Haskell 튜토리얼](https://learn.hfm.io/)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)