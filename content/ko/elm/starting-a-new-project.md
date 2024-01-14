---
title:                "Elm: 새로운 프로젝트 시작하기"
simple_title:         "새로운 프로젝트 시작하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 왜

새로운 프로젝트를 시작하는것의 이유는 매우 다양합니다. 그러나 가장 중요한 이유는 자신의 스킬을 향상시키기 위해서 입니다. 엘름은 함수형 프로그래밍 언어로서, 높은 수준의 추상화와 모듈화를 가능하게 합니다. 또한 클라이언트 사이드 어플리케이션을 작성하는데 있어서 안정성과 확장성을 보장해 줍니다.

## 시작하기

엘름을 사용해서 새로운 프로젝트를 시작하는것은 간단합니다. 먼저, 에디터에서 반드시 엘름 확장을 설치해야 합니다. 그래야지만 코드를 작성하고 컴파일 할 수 있습니다. 그리고 실행하기 위해, 다음과 같은 코드를 작성해야 합니다.

```elm
module Main exposing (main)

import Html exposing (text)

main =
  text "Hello World!"
```

위 코드는 단순하게 "Hello World!"를 출력하는것에 대한 기본적인 예제입니다. 이제 코드를 저장하고 컴파일한 후, 웹 브라우저를 열어서 결과를 확인할 수 있습니다.

## 깊이있게 살펴보기

새 프로젝트를 시작할때, 가장 중요한 부분은 구조를 잘 설계하는 것입니다. 엘름에서는 모듈화를 이용해서 코드를 구조화 할 수 있고 상태관리를 위해 "Model-View-Update" 아키텍처를 사용합니다. 또한 타입 안전성을 보장하기 위해 많은 도구들이 제공되며, 이를 통해 코드의 안정성과 신뢰성을 높일 수 있습니다.

그리고 엘름 커뮤니티에서는 많은 유용한 라이브러리들과 기존의 자바스크립트 라이브러리를 익숙한 엘름 형식으로 감싸준 패키지들을 찾아볼 수 있습니다. 이러한 패키지들을 사용하면 새로운 프로젝트를 더욱 쉽게 시작할 수 있습니다.

## 또 다른 정보

이외에도 엘름에 대해 궁금한 점이 있으시다면 다음 링크들을 참고해주세요.

- [엘름 공식 홈페이지](https://elm-lang.org/)
- [프로그래밍 언어 엘름 (번역서)](https://scenaristeur.github.io/programmierung-elm/)
- [엘름온라인 온라인 편집기](https://ellie-app.com/new)
- [엘름 커뮤니티 포럼](https://discourse.elm-lang.org/)
- [엘름 패키지 관련 정보](https://package.elm-lang.org/)
- [엘름 슬랙 채팅](https://elmlang.herokuapp.com/)