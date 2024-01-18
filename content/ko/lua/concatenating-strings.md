---
title:                "문자열 연결하기"
html_title:           "Lua: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/concatenating-strings.md"
---

{{< edit_this_page >}}

## 달나라에서는 왜 무엇을 하나?
문자열 연결은 문자열을 단일 문자열로 결합하는 것을 말합니다. 프로그래머들은 이를 하는 이유는 자주 사용하는 문자열을 포함하여 더 큰 문자열을 만들기 위해서 입니다.

## 어떻게 하나요?
`Lua`에서는 `..` 연산자를 사용하여 두 문자열을 연결할 수 있습니다. 예를 들어, `Hello `와 `world!` 문자열을 연결하려면 `Hello ` .. `world!`와 같이 작성하면 됩니다.

```Lua
print("Hello " .. "world!")
```
출력: `Hello world!`

또한, 여러 문자열을 연결할 수도 있습니다. 예를 들어, `배고파 `, `하지만 `, `책임지고`와 같은 문자열을 연결하려면 다음과 같이 작성할 수 있습니다.

```Lua
print("배고파 " .. "하지만 " .. "책임지고")
```
출력: `배고파 하지만 책임지고`

## 깊이 파고들어보기
문자열 연결은 프로그래밍에서 매우 중요한 부분이기 때문에 다양한 방식으로 구현되어 왔습니다. 예를 들어, 몇몇 언어에서는 `+` 연산자를 사용하여 문자열을 연결하는 반면, 다른 언어에서는 `concat()` 함수를 제공합니다.

또한, 문자열 연결의 대안으로는 문자열 보간이 있습니다. 언어마다 다르지만, 보간은 변수 값을 문자열에 삽입하여 문자열을 만들 수 있게 해줍니다.

## 더 알아보기
- [Lua 공식 사이트](https://www.lua.org/)
- [Lua 문자열 라이브러리 문서](https://www.lua.org/manual/5.3/manual.html#6.4)
- [다른 언어에서 문자열 연결하는 방법 비교하기](https://en.wikipedia.org/wiki/String_concatenation)