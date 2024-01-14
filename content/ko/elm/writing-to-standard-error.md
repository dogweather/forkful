---
title:    "Elm: 표준 에러에 쓰는 방법"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜

이번 포스트에서는 Elm 프로그래밍의 중요한 측면 중 하나인 표준 에러에 대해 다뤄보겠습니다. 표준 에러에 쓰는 것이 왜 중요한지 이유를 알아보겠습니다.

## 어떻게

표준 에러(standard error)는 Elm 코드에서 오류를 식별하고 디버깅하는 데 매우 유용한 프로그래밍 기법입니다. 다음과 같이 코드 블록을 작성하면 해당 블록 내부에 있는 코드가 표준 에러에 쓰여집니다.

```Elm
-- 코드 예시
import Debug

Debug.log "Hello!" "world"
```

위 코드를 실행하면 "world"라는 문자열이 표준 에러에 출력됩니다. 이렇게 출력된 결과는 디버깅을 위해 사용할 수 있습니다.

## 깊이 들어가기

표준 에러는 Elm 프로그래밍에서 매우 중요하고 유용한 기능입니다. 이를 이용하면 코드에서 발생하는 오류를 더 쉽게 추적하고 디버깅할 수 있습니다. 또한, 표준 에러를 이용하면 코드의 실행 흐름을 더욱 세밀하게 조정할 수 있는 장점도 있습니다.

## 참고 자료

마지막으로 일반 출력과 표준 에러의 차이점과 표준 에러를 활용하는 방법에 대해 더 알고싶다면 아래의 링크를 참고하시기 바랍니다.

- [Elm 표준 에러 문서](https://guide.elm-lang.org/error_handing/errors.html)
- [Debug 모듈 문서](https://package.elm-lang.org/packages/elm/core/latest/Debug)
- [디버깅을 위한 표준 에러 사용하기](https://www.snoyman.com/blog/2019/10/elm-debug#Explicitly_printing_to_stderr)

## 참고하기

- [Elm 표준 출력에 대한 포스트](https://exampleblog.com/elm/standard-output)
- [Elm 디버깅에 대한 팁 포스트](https://exampleblog.com/elm/debugging-tips)