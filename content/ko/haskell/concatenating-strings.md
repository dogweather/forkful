---
title:    "Haskell: 문자열 연결"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

문자열을 연결하는 방법을 배우기 전에, 우리는 왜 이것을 해야하는지에 대해서 이야기해야 합니다. 문자열을 연결하는 것은 컴퓨터 프로그래밍에서 매우 일반적인 작업 중 하나입니다. 예를 들어, 사용자로부터 입력을 받고 그것을 반영하는 프로그램을 만들거나, 다른 파일들의 이름을 합쳐서 새로운 파일을 만드는 등에 사용할 수 있습니다. 이는 간단한 작업처럼 보일 수 있지만, 실제로는 매우 중요한 작업입니다. 그러니까 문자열을 연결하는 방법을 익혀두는 것은 우리에게 도움이 됩니다.

## 어떻게

Haskell을 사용하여 문자열을 연결하는 방법을 배우기 전에, 우리는 먼저 문자열이 어떻게 이루어져 있는지 알아야 합니다. 일반적으로 문자열은 문자들의 리스트로 이루어져 있습니다. 예를 들어, "Hello World"라는 문자열은 'H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd'라는 문자들의 리스트로 이루어져 있습니다.

이제 문자열을 연결하는 방법을 배워봅시다. 우리는 `++` 연산자를 사용하여 두 개의 문자열을 연결할 수 있습니다.

```Haskell
"Hello " ++ "World"
```

위의 코드는 "Hello World"라는 문자열을 생성합니다. 또한, 우리는 문자열을 변수에 저장하고 `++` 연산자를 사용하여 문자열을 연결할 수도 있습니다.

```Haskell
let str1 = "Hello "
let str2 = "World"
str1 ++ str2
```

위의 코드는 "Hello World"라는 문자열을 생성합니다. 이제 다음은 두 개의 변수에 저장된 문자열을 연결하여 새로운 변수에 할당하는 예제입니다.

```Haskell
let name = "Korean"
let greeting = "Hello "
let message = greeting ++ name
```

위의 코드는 "Hello Korean"이라는 문자열을 생성하고 `message` 변수에 할당합니다.

## 깊게 파고들기

Haskell에서 `++` 연산자는 두 개의 문자열을 새로운 문자열로 연결하는 방법 중 하나입니다. 이외에도 문자열을 연결하는 다른 방법들이 존재합니다. 예를 들어, 우리는 `concat` 함수를 사용하여 문자열의 리스트를 연결할 수도 있습니다.

```Haskell
let names = ["Korean", "Chinese", "Japanese"]
concat names
```

위의 코드는 "KoreanChineseJapanese"라는 문자열을 생성합니다. 또한, 우리는 `foldl` 함수를 사용하여 문자열의 리스트를 연결할 수도 있습니다.

```Haskell
let names = ["Korean", "Chinese", "Japanese"]
foldl (++) "" names
```

위의 코드는 "KoreanChineseJapanese"라는 문자열을 생성합니다. 여러분은 위의 예제들을 통해, 문자열을 연결하는 다양한 방법들을 배워볼 수 있을 것입니다.

## 더 알아보기

만약 여러분이 Haskell을 처음 접하신다면, 문자열 연결 외에도 더 많은 것들을 배우고 싶을 수 있을 것입니다. 아래의 링크들을 따라가면 더 많은 정보를 얻을 수 있습니다.

- [Haskell 공식 홈페이지](https://www.haskell.org/)
- [Haskell 튜토리얼](https://www.tutorialspoint.com/haskell/index.htm)
- [Haskell의 문자열 형식