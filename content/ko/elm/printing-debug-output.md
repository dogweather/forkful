---
title:    "Elm: 디버그 출력하기"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## 왜

디버그 출력을 생성하는 데 참여하는 이유는 주로 디버깅과 오류 해결에 도움이 되기 때문입니다.

## 방법

Elm에서 디버그 출력을 생성하려면 "```Elm debug```"를 사용해야 합니다. 아래는 예제 코드와 예상 결과 값입니다.

```Elm

import Debug exposing (log)

log "Hello, world!"

--[Debug]: Hello, world!
```

디버그 출력을 사용하여 변수의 값을 확인할 수도 있습니다.

```Elm
import Debug exposing (log)

name = "Jane"
log name

--[Debug]: Jane
```

## 깊게 파헤치기

디버그 출력에는 여러 가지 유형이 있을 수 있으며 원하는 방식으로 표현할 수 있습니다. 이를테면, 디버그 출력을 출력하는 데 도움이 되는 디버깅 모니터링 도구가 있거나 자신만의 디버그 모니터링 라이브러리를 만들 수도 있습니다. 

## 참고

- Elm 디버깅 문서: https://guide.elm-lang.org/debugging/debugging.html
- Elm 디버깅을 개선하는 방법: https://medium.com/@9boomerang/debunking-elm-debugging-improvements-6a821514f05b
- 디버깅 팁 및 트릭: https://dev.to/3lm/elm-debugging-tips-and-tricks-31h7