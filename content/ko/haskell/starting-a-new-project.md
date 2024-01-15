---
title:                "새로운 프로젝트 시작하기"
html_title:           "Haskell: 새로운 프로젝트 시작하기"
simple_title:         "새로운 프로젝트 시작하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 왜

새 프로젝트를 시작하는 이유는 여러 가지가 있지만 주요한 이유는 첫째로 새로운 언어를 배우고 싶은 욕심일 것입니다. 둘째로 함수형 프로그래밍 언어를 배우는 것이 미래의 컴퓨팅에서 핵심이 될 것이기 때문입니다.

## 어떻게 시작할까요?

우선, Haskell을 설치해야 합니다. 이를 위해 Haskell 플랫폼을 다운로드하고 설치할 수 있습니다. 설치가 완료된 후, `ghci`를 실행해 Haskell 인터프리터를 사용할 수 있습니다.

```Haskell
$ ghci
GHCi, version 8.6.5: http://www.haskell.org/ghc/  :? for help
Prelude>
```
우리는 이제 Haskell을 사용할 수 있습니다. 이제 `Hello World`를 출력하는 간단한 예제를 살펴보겠습니다.

```Haskell
Prelude> putStrLn "Hello World"
Hello World
```

## 딥 다이브

새 프로젝트를 시작할 때 가장 중요한 것은 목표를 정의하는 것입니다. 목표를 정의할 때, 클린 코드를 작성하는 것을 목표로 삼으면 프로젝트를 시작하기 좋습니다.

Haskell은 함수형 언어이기 때문에, 함수를 중심으로 프로그래밍하게 됩니다. 새 프로젝트를 시작할 때 가장 먼저 해야 할 일은 필요한 함수를 작성하는 것입니다. 함수를 작성할 때에는 타입 시그니처를 명시하는 것이 좋습니다. 이는 함수의 입력과 출력에 대한 정보를 제공하기 때문입니다.

또한 Haskell에서는 타입을 명시적으로 지정하는 것이 매우 중요합니다. 타입이 지정되지 않은 코드는 컴파일되지 않기 때문입니다. 따라서, 타입 시스템에 익숙해지는 것은 매우 중요합니다.

간단한 프로그램을 작성할 때는 `ghci`를 사용하여 테스트하는 것을 추천합니다. 그러나 실제 프로젝트를 작성할 때는 Haskell 빌드 도구인 `cabal`이나 `stack`을 사용하는 것이 좋습니다. 이 빌드 도구들은 프로젝트를 관리하고 다양한 패키지를 추가할 수 있게 해주기 때문입니다.

## 참고

- [Haskell 사이트](https://www.haskell.org/)
- [Haskell 플랫폼 다운로드](https://www.haskell.org/platform/)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)
- [Haskell Wikibook](https://en.wikibooks.org/wiki/Haskell)