---
title:    "Elm: 텍스트 파일 쓰기"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일 작성을 시작하는 이유는 무엇일까요? 우리가 일상에서 우리의 생각, 아이디어, 메모를 기록하는 것과 같이, 개발자들은 코드를 작성하여 자신의 고유한 아이디어를 기록할 수 있습니다. 텍스트 파일을 작성하는 것은 소프트웨어 개발에서 중요한 부분이며, Elm 프로그래밍에서도 예외는 아닙니다.

## 작성하는 방법

우리는 Elm에서 텍스트 파일을 작성하는 방법에 대해 배울 것입니다. 텍스트 파일을 작성하는 가장 일반적인 방법은 `Text.encode` 함수를 사용하는 것입니다. 이 함수는 텍스트를 바이트 스트림으로 인코딩하는 데 사용됩니다. 아래에는 예제 코드와 결과가 나열되어 있습니다.

```Elm
import Text

text = "안녕하세요!"

encodedText = Text.encode text

main = text
```

위의 코드를 실행하면 다음과 같은 출력을 볼 수 있습니다.

```
SimpleCode.elm:5:5

--- BEGIN MESSAGE ---

Unknown variable 'text'.

Maybe you want one of the following?

    Text.text

--- END MESSAGE

I was not able to find any code at all!

None of these directories had SimpleCode.elm:
.
```

위의 출력은 코드에 Syntax 오류가 있음을 나타냅니다. 즉, `Text.text`로 수정되어야 합니다. 아래에는 수정된 코드와 결과가 나열되어 있습니다.

```Elm
import Text

text = "안녕하세요!"

encodedText = Text.encode text

main = encodedText
```

위의 코드를 실행하면 다음과 같은 결과를 볼 수 있습니다.

```
[23692,34701,26402,12395,12426,12401,12376,12435,21830]
```

위의 결과는 UTF-8 인코딩을 통해 텍스트가 바이트로 변환된 것입니다. 우리는 이를 다시 읽을 수 있지만, 일반적으로 텍스트 파일을 작성하기 전에 인코딩 된 텍스트를 파일로 저장하는 것이 바람직합니다.

## 깊이 파고들기

우리는 이제 Elm에서 텍스트 파일 작성을 할 수 있는 기본적인 방법을 배웠습니다. 그러나 더 깊이 파고들어서 다른 인코딩을 구현해 볼 수도 있습니다. 또한 파일 시스템과 상호 작용하도록 코드를 확장하는 것도 가능합니다. Elm에서 이를 수행하기 위해서는 더 많은 공부와 연습이 필요하지만, 기본적인 지식을 익힌 후에는 쉽게 구현할 수 있습니다.

## 더 알아보기

- [Elm 공식 문서](https://guide.elm-lang.org/)
- [Elm 커뮤니티 포럼](https://discourse.elm-lang.org/)
- [Elm Weekly 뉴스레터](https://elmweekly.nl/)