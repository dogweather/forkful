---
title:    "Gleam: 문자열 연결"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜 

항상 언어를 사용할 때 우리는 글자 또는 단어를 연결해야하는 경우가 있습니다. 이는 특히 문자열을 처리하는 프로그래밍에서 필수적이며, 올바르게 문자열을 연결하면 코드를 더 깔끔하고 읽기 쉽게 만들 수 있습니다. 그래서 오늘은 Gleam에서 문자열을 연결하는 방법을 알아보겠습니다.

## 어떻게 

```Gleam
// 두 개의 문자열을 간단하게 연결하는 방법
let message = "안녕하세요" <> "Gleam!"

// 변수나 함수의 반환값과 결합하는 방법
let name = "John"
let greeting = message <> name

// 여러 개의 문자열을 연결하는 방법
let sentence = "제 이름은 " <> name <> "이고, " <> message <> " " <> "저는 프로그래머입니다."
```

위의 예제 코드를 보면 <code><></code> 연산자를 사용하여 두 문자열을 쉽게 연결할 수 있다는 것을 알 수 있습니다. Gleam에서는 문자열을 마치 다른 기본 자료형처럼 취급하기 때문에, 더 복잡한 계산식이나 변수나 함수의 반환값과도 결합할 수 있습니다.

## 깊게 파헤치기 

만약 여러분이 문자열을 연결하는데 있어서 더 깊이 파고들고 싶다면, Gleam에서 제공하는 문자열 라이브러리를 살펴보시는 것이 좋습니다. <code>gleam/string</code> 라이브러리에는 문자열을 처리하기 위한 다양한 함수들이 포함되어 있으며, 이를 이용해 더욱 다양한 기능을 구현할 수 있습니다. 예를 들어, <code>gleam/string</code> 라이브러리의 <code>join</code> 함수는 리스트 안의 문자열들을 모두 결합해 하나의 문자열로 만들어줍니다. 또한 <code>gleam/string_builder</code> 모듈을 사용하면 문자열을 효율적으로 추가하고 수정할 수 있습니다.

## 더 알아보기 

Gleam에서 문자열을 다루는 것 외에도, 다른 유용한 기능들을 알아보세요! 아래 링크들을 참고하시면 프로그래밍에 더욱 능숙해질 수 있을 것입니다.

- [Gleam 공식 문서](https://gleam.run/documentation)
- [Gleam 커뮤니티 포럼](https://discord.gg/gleam-lang)
- [무료 온라인 과정 - Gleam 프로그래밍 입문](https://www.bitdegree.org/course/gleam-programming)