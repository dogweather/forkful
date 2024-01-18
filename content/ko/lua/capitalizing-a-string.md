---
title:                "문자열 대문자로 바꾸기"
html_title:           "Lua: 문자열 대문자로 바꾸기"
simple_title:         "문자열 대문자로 바꾸기"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 문자열의 대소문자 변환 - 왜 해야 하나
문자열을 대소문자로 변환한다는 것은 단어의 첫 글자를 대문자로 만들고, 나머지는 소문자로 만든다는 것을 의미합니다. 프로그래머들은 이 작업을 주로 하는데, 사용자로부터 입력받는 정보나 저장된 데이터를 일관성있게 처리하기 위해서입니다.

## 대소문자 변환 방법:
시작하기 전에, 대소문자 변환 함수를 불러오기 위해 두 가지 방법이 있습니다. 첫 번째 방법은 문자열의 첫 글자를 대문자로 만들기 위해 `string.upper()` 함수를 사용하는 것입니다. 예를 들어, `string.upper("hello")`는 "HELLO"를 반환합니다. 두 번째 방법은 문자열을 모두 소문자로 만들기 위해 `string.lower()` 함수를 사용하는 것입니다. 예를 들어, `string.lower("HELLO")`는 "hello"를 반환합니다.

```Lua
-- 대문자로 변환 예제
print(string.upper("hello")) --> HELLO

-- 소문자로 변환 예제
print(string.lower("HELLO")) --> hello
```

## 조금 더 깊게 들어가보기:
대소문자 변환은 옛날부터 사용되어 왔습니다. 예를 들어, 기계어나 어셈블리어에서는 대문자와 소문자를 구분하지 않기 때문에 대소문자의 사용이 중요하지 않았습니다. 하지만 지금은 대소문자를 구분하는 프로그래밍 언어에서 다양한 방식으로 사용됩니다. 예를 들어, 몇몇 언어에서는 문자열을 대소문자 구분 없이 비교하기 위해 대소문자 변환 함수를 사용합니다. 또한 URL에서도 대소문자를 구분하지 않기 때문에, 엔지니어들은 대소문자를 구분하지 않는 URL 라우팅 시스템을 만들기 위해 대소문자 변환 기능을 종종 사용합니다.

## 관련 자료:
- [Lua 공식 문서 - string 라이브러리](http://www.lua.org/manual/5.3/manual.html#6.4)
- [파이썬 string 모듈](https://docs.python.org/3/library/string.html)