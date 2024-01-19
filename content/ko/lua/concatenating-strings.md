---
title:                "문자열 연결하기"
html_title:           "Arduino: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무엇이며 왜합니까?

문자열 연결은 여러 개의 문자열을 하나로 합치는 과정입니다. 프로그래머들은 이를 사용하여 프로그램 출력을 맞추거나 깔끔하게 조직하는 등 다양한 목적으로 사용합니다.

## 어떻게 하는가:

Lua에서 문자열을 연결하는 방법을 다음 코딩 예시를 통해 보도록 하겠습니다.

```Lua
str1 = "Hello, "
str2 = "World!"
result = str1 .. str2
print(result)
```

이 코드는 "Hello, "와 "World!"를 연결하여 "Hello, World!"를 출력합니다.

## 깊은 내용:

### 역사적 배경

Lua에서의 문자열 연결은 C 계열 언어와 다르게 `..` 기호를 사용합니다. 이는 초기 Lua 설계자들이 라틴 문자열 연산에서 영감을 받아, 프로그램에서 사용되는 일반적인 '+' 연산자와 구분하기 위해 도입된 방법입니다.

### 대안

문자열 형식 함수인 `string.format` 또한 문자열 연결에 사용될 수 있습니다. 이는 특히 복잡한 문자열 서식 지정이 필요한 경우 유용합니다.

```Lua
str1 = "Hello, "
str2 = "World!"
result = string.format("%s%s", str1, str2)
print(result)
```

### 구현 세부사항

Lua에서 문자열 연결을 사용할 때 주의해야 할 기억해야 할 점은, 문자열 자체는 불변(immutable)하다는 것입니다. 즉, 두 문자열을 연결하면 새 문자열이 생성되며, 원본 문자열은 변경되지 않습니다.

## 연관 참고자료:

- Lua 공식 설명서의 문자열 연산 부분: [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/manual.html#3.4.5)
- Lua 문자열 함수와 연산에 대한 좋은 튜토리얼: [Learn Lua](https://www.learn-lua.org/en/Strings)