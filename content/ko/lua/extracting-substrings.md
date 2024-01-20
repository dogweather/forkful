---
title:                "부분 문자열 추출"
html_title:           "Arduino: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/extracting-substrings.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열 추출은 문자열에서 특정 부분을 가져오는 것입니다. 이는 데이터를 처리하고 분석할 때 중요한 작업 중 하나로, 효율적인 프로그래밍을 가능케 합니다.

## 사용법:
Lua에서 문자열 추출은 `string.sub()` 함수를 사용합니다. 여기 예시 코드와 결과입니다:

```Lua
str = "안녕하세요, Lua 프로그래밍"
print(string.sub(str, 1, 5))
```
결과:
```Lua
안녕하세요
```
위의 예제에서는 문자열 `str`에서 첫 번째 문자로부터 다섯 번째 문자까지의 부분 문자열을 추출하였습니다.

## 깊게 알아보기
문자열 추출은 프로그래밍의 역사와 함께한 기능 중 하나로, 메모리 관리와 데이터 처리에 있어서 중요한 역할을 합니다. Lua에서는 `string.sub()` 외에도 `string.find()` 등 특정 문자열을 찾는 함수도 제공하고 있습니다.

구현 면에서 보면, `string.sub()` 함수는 Lua에서 제공하는 문자열 라이브러리의 일부로 작동합니다. 이 함수는 시작 인덱스와 종료 인덱스를 받아 대상 문자열에서 선택된 범위의 부분 문자열을 반환합니다.

## 참고자료
다음은 문자열 처리와 관련된 일부 외부 자료의 링크입니다:

[1] 공식 Lua 문자열 라이브러리 문서: (https://www.lua.org/manual/5.3/manual.html#6.4)

[2] Lua 프로그래밍 가이드의 문자열 처리 섹션: (https://www.lua.org/pil/20.html)

[3] Lua 문자열 함수에 대한 튜토리얼 (englisch): (https://www.tutorialspoint.com/lua/lua_strings.htm)