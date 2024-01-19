---
title:                "새 프로젝트 시작하기"
html_title:           "Arduino: 새 프로젝트 시작하기"
simple_title:         "새 프로젝트 시작하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Haskell과 새로운 프로젝트 시작하기

## 무엇 & 왜?

새로운 프로젝트 시작하기란 개발자들이 적정한 코드 구조를 만들어 새로운 아이디어나 기능을 구현하기 위한 초기 단계입니다. 이를 통해 효율적인 코드 작성과 더 나은 유지보수를 위한 기반을 마련합니다. 

## 어떻게 하나:

`stack`을 통해 새로운 프로젝트를 시작해보겠습니다. 먼저, 콘솔에서 아래 명령을 실행해봅니다.

~~~Haskell
stack new my-cool-project
~~~

이 명령은 `my-cool-project`라는 새로운 디렉토리를 생성하고, 기본 프로젝트 파일들을 생성합니다.

이제 프로젝트에 들어가 함수를 작성해 봅시다.

~~~Haskell
module Main where

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
~~~

프로젝트를 빌드하려면 아래 명령을 입력합니다.

~~~Haskell
stack build
~~~

프로그램을 실행하면 "Hello, Haskell!"이 출력됩니다.

## 깊이있는 다루기

사실, Haskell 프로젝트를 시작하는 방법은 여러가지가 있습니다. `stack`은 그 중 하나로, 기본적인 틀을 제공하며 라이브러리 관리를 쉽게 해줍니다. `stack`은 2015년에 출시된 다음 세대 Haskell 도구입니다. 이전에는 `cabal`이 주로 사용되었지만, 설정 트러블 이슈로 인해 더 새로운 도구가 필요했습니다.

`cabal`이나 순수한 `ghc`를 이용하는 방법도 있지만, 시작하기 쉽도록 프로젝트 플랫폼으로 `stack`를 권장합니다. 따라서 여러분들도 `stack`을 이용해서 쉽게 Haskell 프로젝트를 시작할 수 있을 것입니다.

## 참고자료

- Haskell 공식 문서: https://www.haskell.org/documentation/
- Stack 공식 문서: https://docs.haskellstack.org/en/stable/README/
- Stack vs Cabal 비교: https://www.stackbuilders.com/news/the-real-difference-between-stack-and-cabal
- GHC 공식 문서: https://www.haskell.org/ghc/