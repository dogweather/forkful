---
title:                "디버그 출력을 인쇄하기"
html_title:           "Clojure: 디버그 출력을 인쇄하기"
simple_title:         "디버그 출력을 인쇄하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## 무엇 그리고 왜?

디버그 출력은 프로그래밍에서 중요한 부분입니다. 이것은 개발자가 프로그램의 특정 부분이 어떻게 작동하는지 파악하는 데 도움이 됩니다. 이런 방식으로 문제점을 찾아내고 수정할 수 있습니다.

## 사용법

여기서는 'Debug.Trace' 모듈을 사용하여 표준 오류에 디버그 정보를 출력하는 방법을 보여줍니다.

```Haskell
import Debug.Trace

main :: IO ()
main = trace "This will be printed before the main function" (return ())
```

위 프로그램은 "This will be printed before the main function" 메세지를 출력 후 아무런 동작도 수행하지 않고 종료합니다.

## 깊은 이해

디버그 출력은 프로그래밍의 오랜 역사와 함께 왔습니다. 시작부터 거의 모든 언어가 이런 기능을 제공했습니다. Haskell에서는 이러한 디버그 출력을  `Debug.Trace` 모듈로 제공하고 있습니다.

이 방법의 단점은 디버그 정보가 표준 오류로 출력되므로, 프로그램을 통제된 환경에서 실행해야만 합니다.

그러나 이런 문제를 해결하기 위한 다른 방법들도 있습니다. 예를 들어, Haskell에는 디버깅을 돕는 여러 라이브러리가 있습니다. 가령, "ghcid"은 파일이 변경될 때마다 자동으로 코드를 재컴파일하며 발생한 오류 또는 경고를 즉시 표시합니다.

## 그밖에 참고하면 좋을 것들

디버그 기술에 대한 더 깊은 이해를 원한다면, 이러한 참고자료들이 도움이 될 것입니다:

- Real World Haskell: 디버깅 공부에 대한 좋은 시작점입니다. [링크](http://book.realworldhaskell.org/read/debugging-and-profiling.html)
- Hackage: Debug.Trace 모듈에 대한 자세한 문서를 제공합니다. [링크](http://hackage.haskell.org/package/base-4.14.1.0/docs/Debug-Trace.html)
- Haskell Wiki: 디버깅에 대한 다양한 팁과 트릭이 제공됩니다. [링크](https://wiki.haskell.org/Debugging)