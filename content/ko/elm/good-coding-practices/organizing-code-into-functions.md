---
title:                "코드를 함수로 구성하기"
aliases:
- /ko/elm/organizing-code-into-functions/
date:                  2024-01-26T01:10:09.721109-07:00
model:                 gpt-4-1106-preview
simple_title:         "코드를 함수로 구성하기"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## 무엇이며 왜인가?
모든 코드를 하나의 큰 더미로 쌓아두는 것은 나쁜 생각입니다. 함수로 코드를 나누는 것은 좋은 생각입니다. 이것은 엘름 코드를 깨끗하게 유지할 수 있게 하고, 재사용 가능하며, 테스트하기 쉽게 만듭니다. 코드를 함수로 구성함으로써, 특정 작업을 수행하는 코드끼리 묶게 되어, 애플리케이션을 더 유지하기 좋고 이해하기 쉽게 만들어줍니다.

## 방법:
다음은 사용자에게 인사하는 간단한 함수가 포함된 엘름 코드의 한 조각입니다:

```Elm
module Main exposing (..)

import Html exposing (text)

greetUser : String -> String
greetUser userName =
    "Hello, " ++ userName ++ "!"

main =
    text (greetUser "Casey")
```

이 코드를 실행하면 출력으로 "Hello, Casey!"가 나옵니다.

이제 개인화를 더하고 싶다고 가정합시다. 기능을 더 추출하세요!

```Elm
module Main exposing (..)

import Html exposing (text)

greetUser : String -> String -> String
greetUser greeting userName =
    greeting ++ ", " ++ userName ++ "!"

personalGreeting : String -> String
personalGreeting userName =
    greetUser "Howdy" userName

main =
    text (personalGreeting "Casey")
```

이제 실행하면: "Howdy, Casey!" 마법일까요? 아니요, 그저 함수가 그들의 일을 하는 것이죠.

## 심층 탐구
옛날에는 코드가 종종 한 줄기의 긴 명령어 시퀀스였습니다(스파게티 코드를 생각해보세요). 유지보수하기에 악몽이었습니다. 그러다 구조화된 프로그래밍이 등장하였고, 그것과 함께 함수가 왔습니다. 엘름은 기능성 프로그래밍의 선조들처럼, 조직 구성을 위해 함수에 크게 의존합니다.

함수를 중첩하여 클로저를 만들 수 있고, 순수함을 유지하기 위해 그것들을 유지할 수 있습니다. 엘름은 후자를 권장합니다: 잘 정의된 입력과 출력을 가진 순수 함수로, 디버깅과 테스트를 더 쉽게 만듭니다.

엘름의 함수는 고차 함수일 수도 있는데, 이는 다른 함수를 받거나 반환할 수 있다는 것을 의미합니다. 이는 결합성의 새로운 세계를 열어줍니다. 그러나, 다른 일부 언어와 달리, 엘름에는 함수 오버로딩이 없으며; 각 함수는 고유한 이름을 가져야 합니다.

추가적으로, 엘름은 타입을 검사할 뿐만 아니라 추론하는 강력한 정적 타이핑 시스템을 적용합니다, 이는 보일러플레이트 코드를 줄입니다.

절차적이거나 객체지향적인 코드 조직과 같은 다른 언어의 대안들에 비교했을 때, 엘름의 접근 방식은 단순성과 예측 가능성을 강조합니다. 엘름에는 객체나 클래스가 없습니다. 클래스와 인스턴스 대신 함수와 모듈로 코드를 구성합니다.

## 또한 보기
더 깊이 탐구하고 싶다면, 이러한 자료들을 확인해 보세요:
- 함수에 대한 엘름 공식 가이드: https://guide.elm-lang.org/core_language.html
- 복잡한 함수 예제에 대한 엘름 패키지 문서: https://package.elm-lang.org/
- 함수 조직과 잘 어울리는 엘름의 타입 시스템에 대해 알아보기: https://elm-lang.org/docs/types
