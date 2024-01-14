---
title:    "Elm: 새 프로젝트 시작"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 왜 시작해야 할까요?

새 프로젝트를 시작하려는 이유는 다양할 수 있습니다. 더 나은 코드를 작성하고 다른 언어에 비해 더 안전하고 예측 가능하며 확장 가능한 앱을 만들 수 있기 때문입니다. 그리고 무엇보다도, Elm 커뮤니티는 매우 친절하고 지원을 제공하기 때문에 새로운 프로젝트를 시작하는 것은 더 쉽고 즐거운 경험이 될 것입니다.

## 어떻게 시작할까요?

먼저 Elm 플랫폼을 설치해야 합니다. 그 후, ```elm init``` 명령어로 새 프로젝트를 초기화할 수 있습니다. 그리고 이제 새로운 코드를 작성해보겠습니다.

```Elm
module Main exposing (main)

import Html

main =
  Html.text "안녕하세요! 이것은 새로운 Elm 프로젝트입니다."
```

이 코드를 실행하면 다음과 같은 출력이 나타납니다.

```
안녕하세요! 이것은 새로운 Elm 프로젝트입니다.
```

## 깊이 파고들기

새로운 프로젝트를 시작할 때, Elm의 중요한 특징 중 하나는 모듈 시스템입니다. 이는 코드를 모듈 단위로 나누어 관리하는 것을 의미합니다. 또한 Elm에서는 모든 값이 불변(immutable)이기 때문에 예기치 않은 버그를 방지할 수 있습니다.

또 다른 중요한 기능은 Elm의 타입 시스템입니다. 이는 코드를 작성하는 동안 타입을 검증하고 맞지 않는 타입 에러를 방지해줍니다. 이는 코드의 안정성과 예측 가능성을 높여주는 중요한 기능입니다.

더 깊이 파고들기 위해선, 공식 Elm 가이드를 살펴보세요. 또한 Elm 커뮤니티에서는 다양한 예제와 자료를 제공해줍니다. 새로운 프로젝트를 시작하기 전에, 이러한 리소스들을 활용하여 더 익숙해지는 것을 추천드립니다.

## 더 알아보기

- [공식 Elm 가이드](https://guide.elm-lang.org/)
- [Elm 커뮤니티 포럼](https://discourse.elm-lang.org/)
- [Elm 슬랙 채널](https://elmlang.herokuapp.com/)

## 참고 자료

Elm을 처음 시작하는데 도움이 될 수 있는 몇 가지 참고 자료들입니다.

- [Elm 플랫폼 설치 가이드](https://guide.elm-lang.org/install.html)
- [Elm 언어 소개](https://guide.elm-lang.org/architecture/)
- [Elm REPL(Read-Eval-Print Loop) 사용하기](https://elm-lang.org/examples/hello)
- [Elm 파일 구조 설명](https://elm-lang.org/guide/architecture/importing-code)
- [그라운드 제로 Elm 시작하기](https://www.elm-tutorial.org/)