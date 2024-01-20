---
title:                "표준 에러에 쓰는 방법"
html_title:           "Elm: 표준 에러에 쓰는 방법"
simple_title:         "표준 에러에 쓰는 방법"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

표준 오류에 쓰는 것은 프로그래머들이 오류 메시지를 확인하고 디버깅하는 데 도움이 됩니다. 표준 출력과 달리 표준 오류는 브라우저에 표시되지 않고 콘솔에서만 확인할 수 있습니다.

## 어떻게?

```elm
import Error
Error.raise "Something went wrong!"
```

이 코드를 실행하면 콘솔에 "Something went wrong!"라는 오류 메시지가 출력됩니다.

## 깊이 파고들기

표준 오류에 쓰는 것은 오래된 디버깅 기술 중 하나입니다. 이전에는 개발자들이 특정 문제가 발생했을 때 오류 메시지를 확인할 수 있도록 메모리에 출력을 보냈습니다. 하지만 이제는 개발자 도구와 같은 도구를 사용하여 표준 출력을 확인할 수 있으므로 표준 오류에 쓰는 것은 더 이상 꼭 필요한 기술은 아닙니다. Elm에서는 ```Debug.log``` 함수를 사용하여 표준 출력에 로그를 남기는 것도 가능합니다.

## 관련 자료

- [Elm 공식 문서](https://guide.elm-lang.org/error_handling/)
- [JavaScript의 표준 오류에 대한 설명](https://developer.mozilla.org/ko/docs/Web/API/Console/error)