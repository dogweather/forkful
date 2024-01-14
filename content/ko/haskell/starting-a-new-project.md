---
title:    "Haskell: 새 프로젝트 시작하기"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 왜

새로운 프로젝트를 시작하는 이유는 다양합니다. 어떤 사람들은 자신의 스킬을 발전시키기 위해 새로운 언어나 프레임워크에 도전하고 싶어 할 것입니다. 또 다른 사람들은 현재 사용 중인 기존 솔루션보다 더 효율적이고 강력한 도구를 찾고 있을 것입니다. 어떤 이유로든 새로운 프로젝트를 시작하는 것은 항상 도전적이고 흥미로운 일이 될 수 있습니다.

## 어떻게

Haskell은 함수형 프로그래밍 언어로서 매우 강력하고 표현력이 뛰어납니다. 새로운 프로젝트를 시작하기 위해서는 몇 가지 단계를 따라야 합니다.

### 프로젝트 만들기

우선 프로젝트의 디렉토리를 만듭니다. 다음으로 `stack new` 명령어를 사용하여 기본적인 프로젝트를 생성합니다.

```Haskell
stack new my-project
```

이 명령어는 `my-project` 디렉토리를 생성하고 기본적인 프로젝트를 설정합니다.

### 모듈 만들기

프로젝트 내에서 사용할 모듈을 만듭니다. 모듈은 프로젝트의 코드를 구조화하는 데 유용합니다. 모듈을 만들기 위해서는 `src` 디렉토리 안에 `MyModule.hs` 파일을 생성합니다.

```Haskell
module MyModule
  ( someFunction
  , anotherFunction
  ) where

someFunction :: Int -> Int
someFunction = ...

anotherFunction :: String -> String
anotherFunction = ...
```

### 메인 함수 만들기

프로젝트의 주 실행 함수를 만듭니다. `main.hs` 파일을 `app` 디렉토리 안에 생성하고 `main` 함수를 작성합니다.

```Haskell
module Main where

import MyModule

main :: IO ()
main = do
  putStrLn "Hello, World!"
  let result = someFunction 42
  print result
```

### 빌드 및 실행

마지막으로 `stack` 명령어를 사용하여 프로젝트를 빌드하고 실행합니다.

```Haskell
stack build
stack exec my-project
```

위의 예제 코드는 "Hello, World!"를 출력하고 `someFunction`을 실행한 결과를 출력합니다.

## 딥 다이브

새로운 프로젝트를 시작할 때 가장 중요한 것은 기본적인 설계와 구조를 잘 세우는 것입니다. Haskell은 자유롭게 기능을 나누고 모듈화할 수 있도록 유연한 구조를 제공합니다. 또한 타입 시스템을 통해 코드의 안전성을 보장하므로 더 나은 유지 보수성과 가독성을 제공할 수 있습니다. 새로운 프로젝트를 시작할 때는 기본적인 컨셉과 설계를 확실히 하고 그에 맞는 모듈 구조를 생각하는 것이 중요합니다.

## 참고 자료

- [Haskell 공식 웹사이트](https://www.haskell.org/)
- [Haskell 튜토리얼 - Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)
- [Haskell 샘플 프로젝트 - Real World Haskell](http://book.realworldhaskell.org/)