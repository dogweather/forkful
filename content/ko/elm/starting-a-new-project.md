---
title:    "Elm: 새 프로젝트 시작하기"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## 왜
왜 새 프로젝트를 시작하는 것이 좋을까요?

새 프로젝트를 시작하는 것은 새로운 챌린지를 받아들일 준비가 되었기 때문입니다. 또한 새로운 언어를 배우고 새로운 기술을 습득하는 기회가 될 수 있습니다. 또한 Elm은 간결하고 안정적인 함수형 언어로서 다양한 프로젝트에 유용하게 활용될 수 있습니다.

## 시작하는 법
Elm으로 간단한 웹 애플리케이션을 만드는 방법을 알아보겠습니다. 먼저 Elm의 구조를 살펴보겠습니다. Elm은 모듈 형식으로 구성되어 있으며, `main` 모듈은 모든 Elm 애플리케이션의 시작점입니다. 이 모듈 안에는 `main`이라는 이름의 함수가 있어야 합니다.

```
module Main exposing (main)

main : Program () () ()
main =
   -- 여기에 코드를 작성하세요
```

다음으로는 기본적인 HTML을 생성하는 `view` 함수를 정의해보겠습니다. 이 함수는 `Html.msg`라는 모듈에서 가져온 `p` 요소를 생성하고 이를 `body` 요소로 감싸서 반환합니다.

```
view : Html msg
view =
   Html.body
      [ Html.text "이것은 새 Elm 프로젝트입니다."
      ]
```

마지막으로 `main` 함수 안에서 `view` 함수를 호출하고 `Browser.sandbox`를 이용하여 브라우저에서 실행할 수 있는 앱을 만들어보겠습니다.

```
module Main exposing (main)

import Browser
import Html

main : Program () ()
main =
   Browser.sandbox
      { init = ()
      , view = view
      , update = \msg model -> (model, Cmd.none)
      }

view : () -> Html msg
view _ =
   Html.body
      [ Html.text "이것은 새 Elm 프로젝트입니다."
      ]
```

이제 브라우저에서 `elm reactor` 명령을 실행하고 브라우저에서 `http://localhost:8000/main.elm`을 열면 새 프로젝트가 실행되는 것을 확인할 수 있습니다.

```
elm reactor
http://localhost:8000/main.elm
```

## 더 깊게 들어가기
새 프로젝트를 시작하면서 가장 중요한 것은 어떤 언어를 사용하느냐보다도 어떻게 코드를 구성하느냐입니다. Elm은 모듈 형식으로 구성되어 있으며, 각 모듈은 필요한 기능들을 잘 나누고 추상화해서 구현해야 합니다. 또한 Elm에서는 부작용을 다루기 위해 `Cmd`와 `Sub`를 이용해야 합니다. 이를 통해 코드를 알아보기 쉽고 유지 보수하기 쉽게 만들 수 있습니다. 또한 Elm 커뮤니티에서는 다양한 패턴과 라이브러리를 제공하고 있으니 참고하시길 바랍니다.

## 이어서 보기
- [Elm 공식 문서](https://guide.elm-lang.org/)
- [Elm Korea 페이스북 그룹](https://www.facebook.com/groups/elmkorea)
- [Elm 커뮤니티 뉴스레터](https://elmweekly.nl/)