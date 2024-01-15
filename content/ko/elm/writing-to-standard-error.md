---
title:                "표준 에러를 작성하기"
html_title:           "Elm: 표준 에러를 작성하기"
simple_title:         "표준 에러를 작성하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜

오늘 우리는 Elm 프로그래밍 언어에서 한 가지 흥미로운 주제를 다룰 것입니다. 바로 Standard Error에 대한 글쓰기입니다. Standard Error는 코드 디버깅과 에러 처리에 매우 중요한 역할을 합니다. 따라서 우리는 Elm에서 Standard Error를 어떻게 쓸 수 있는지 살펴볼 것입니다.

## 방법

Standard Error에 대한 글쓰기는 매우 쉽습니다. Elm에서 `Debug` 모듈을 불러와서 `toString` 함수를 이용하면 됩니다. 예를 들어서 우리는 다음과 같은 코드를 작성할 수 있습니다:

```Elm
import Debug

Debug.toString "Hello World"
```

이 코드를 실행하면 `Hello World`라고 콘솔에 출력됩니다. 이렇게 코드를 작성하면 우리는 어떤 변수나 값의 상태를 확인할 수 있습니다. 또한 `toString` 함수는 자바스크립트와는 다르게 더 많은 정보를 보여줍니다. 예를 들어서 리스트의 경우에는 모든 요소들이 출력되는 것을 볼 수 있습니다.

## 깊이 들어가기

`Debug` 모듈을 사용하여 Standard Error에 대한 기본적인 기능을 확인했습니다. 이제는 좀 더 깊이 들어가서 코드를 좀 더 정교하게 작성하는 방법을 알아보겠습니다.

첫 번째로 우리는 람다 함수를 사용하여 좀 더 간단하게 코드를 작성할 수 있습니다. 예를 들어서 `toString` 함수를 사용하지 않고 `Debug.toString` 대신 `(x) -> Debug.toString x`와 같이 작성할 수 있습니다. 이렇게 하면 코드가 좀 더 간결해지고 가독성도 좋아집니다.

또한 우리는 `Debug` 모듈로부터 불러온 함수만 사용하면 됩니다. 그렇지 않으면 불필요한 메모리 공간을 차지하는 `import Debug` 문장을 작성하게 됩니다.

마지막으로 우리는 `Debug` 모듈 대신 `Console` 모듈을 사용할 수도 있습니다. `Console` 모듈은 `Debug` 모듈보다 더 다양한 기능을 제공하고 더 많은 커스터마이징이 가능합니다. 더 자세한 내용은 [Elm 공식 문서](https://guide.elm-lang.org/error_handling/console.html)를 참고하시면 됩니다.

## 참고

[Ellie Online Editor](https://ellie-app.com/) - Elm 코드를 온라인에서 실행하는 무료 툴
[Elm 공식 홈페이지](https://elm-lang.org/) - Elm 언어에 대한 정보와 문서를 제공합니다. 
[Elm 코딩 초보자를 위한 가이드](https://guide.elm-lang.org/) - Elm 언어를 처음 시작하는 분들을 위한 가이드입니다.