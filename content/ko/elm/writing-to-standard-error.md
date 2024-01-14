---
title:                "Elm: 표준 오류에 쓰는 방법"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 왜

왜 우리는 Elm에서 표준 오류에 쓰는 일에 관심이 있을까요? 여러분이 작업하는 코드 중에 오류가 발생하게 되면, 프로그램이 중단되고 오류 메시지가 출력됩니다. 이때 표준 오류에 쓰는 것은 이 오류를 파악하고 디버깅하는 데에 매우 유용합니다.

## 어떻게 하나요?

우리는 ```Debug``` 모듈과 함께 오류를 출력할 수 있습니다. 이를 위해서는 먼저 해당 모듈을 ```import```해야 합니다. 그리고 ```Err``` 함수를 사용하여 오류를 생성하고, ```log``` 함수를 사용하여 해당 오류를 출력할 수 있습니다.

```Elm
import Debug

Debug.log "오류 메시지" (Debug.crash "이 오류를 파악하기 위한 내용")
```

위의 코드를 실행하면, 표준 오류에 다음과 같은 출력이 됩니다.

```bash
오류 메시지: 이 오류를 파악하기 위한 내용
```

## 깊게 들어가보기

왜 우리는 ```Debug``` 모듈을 사용해야 하는 걸까요? 이 모듈을 사용하면, 프로그램이 컴파일되지 않더라도 오류 메시지를 출력할 수 있습니다. 이는 디버깅에 매우 유용하며, 오류를 찾는 데에 큰 도움이 됩니다.

또한 ```Debug.userError``` 함수를 사용하여, 사용자 정의 오류 메시지를 출력할 수도 있습니다. 이 함수는 사용자가 원하는 오류 메시지를 생성하여, 이를 표준 오류에 출력합니다.

더 자세한 정보는 Elm 문서를 참고해주세요.

# 더 알아보기

더 많은 정보를 찾고 싶다면, 아래 링크를 참고해주세요.

- [Elm 문서 - 표준 오류 출력하기](https://guide.elm-lang.org/error_handling/debugging.html)
- [Elm 디버깅 툴 - 시각적인 디버깅을 위한 도구](https://github.com/jheisenbug/elm-debug)
- [Elm 슬랙 - 코더들간의 소통 공간](https://elmlang.herokuapp.com/)