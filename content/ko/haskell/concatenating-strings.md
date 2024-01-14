---
title:                "Haskell: 문자열 연결하기"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜?

문자열 합치기를 하는 이유는 프로그래밍에서 유용하기 때문입니다. 문자열을 합치면 원하는 형식으로 새로운 문자열을 만들 수 있고, 데이터를 처리하거나 출력할 때 매우 편리합니다.

## 어떻게?

Haskell에는 문자열 합치기를 위한 여러 가지 방법이 있습니다. 가장 간단한 방법은 `++` 연산자를 사용하는 것입니다. 예를 들어, "Hello"와 "World"라는 두 문자열을 합치면 "Hello World"가 됩니다. 이를 아래의 코드 블록을 통해 확인해보세요.

```Haskell
"Hello" ++ "World"
```

Output:
"Hello World"

또 다른 방법은 `concat` 함수를 사용하는 것입니다. 이 함수는 리스트 안에 있는 모든 문자열을 합쳐 하나의 문자열로 만들어줍니다. 아래의 코드 블록을 통해 확인해보세요.

```Haskell
concat ["Learn", " ", "Haskell"]
```

Output:
"Learn Haskell"

여러 개의 문자열을 합칠 때는 `$` 연산자를 사용해서 중첩된 괄호 없이도 쉽게 합칠 수 있습니다. 이를 아래의 코드 블록을 통해 확인해보세요.

```Haskell
"Hello" ++ " " ++ "World"
```

Output:
"Hello World"

또 다른 방법으로는 `foldl` 함수를 사용하는 것입니다. 이 함수는 리스트 안에 있는 숫자를 더할 때 처럼, 문자열을 합칠 때도 사용할 수 있습니다. 아래의 코드 블록을 통해 확인해보세요.

```Haskell
foldl (++) "" ["Hello", " ", "World"]
```

Output:
"Hello World"

## 깊이 들어가기

Haskell에서 문자열은 단순히 문자들의 리스트로 간주됩니다. 따라서 문자열을 합칠 때에는 리스트 안에 있는 원소들을 합치는 것과 동일한 방식으로 진행됩니다. `++` 연산자나 `concat` 함수는 내부적으로 `foldl` 함수를 사용하기 때문에, 이 또한 리스트의 각 항목들을 합쳐가며 최종적으로 문자열을 만들어냅니다.

## 더 알아보기

* [Learn You a Haskell](http://learnyouahaskell.com/chapters) - 이 사이트에는 Haskell의 기초부터 고급 개념까지 다양한 내용이 담겨있습니다.
* [Real World Haskell](http://book.realworldhaskell.org/) - 이 책은 실제로 사용 가능한 어플리케이션을 만들기 위한 실용적인 정보를 제공합니다.
* [Haskell Wiki](https://wiki.haskell.org/) - 헤스켈에 관련된 다양한 정보와 자료를 제공하는 위키입니다.

---

## 관련 링크

* [Haskell 공식 홈페이지](https://www.haskell.org/)
* [Haskell 한국 사용자 협회](http://www.haskell.kr/)