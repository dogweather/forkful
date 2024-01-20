---
title:                "문자열에서 날짜 분석하기"
html_title:           "Gleam: 문자열에서 날짜 분석하기"
simple_title:         "문자열에서 날짜 분석하기"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 파싱이 무엇이며, 왜 필요한가?

문자열에서 날짜를 파싱하는 것은 문자열 데이터를 날짜 형식으로 변환하는 과정을 말합니다. 프로그래머는 이런 방식으로 사용자가 입력한 날짜 정보를 적절하게 이해하고 처리할 수 있습니다.

## 어떻게 해야 할까요?

Lua언어에서 다행히도 이런 작업들은 매우 간단합니다. 아래 예제를 살펴보세요.

```Lua
date = "2022-01-27"
year, month, day = date:match("(%d+)%-(%d+)%-(%d+)")
print(year)  -- Output: 2022
print(month) -- Output: 01
print(day)   -- Output: 27
```

위 스크립트는 match 함수를 사용하여 "YYYY-MM-DD" 형식의 날짜를 파싱합니다.

## 심층 이해를 위해

역사적으로, 문자열에서 날짜 정보를 추출하는 방법은 다양합니다. 과거에는 문자열 조작 함수를 사용하여 수동으로 날짜를 추출하는 경우가 많았습니다. 하지만 이는 복잡하며 버그 생성 가능성이 높습니다. 

또한, 다른 프로그래밍 언어에는 이미 날짜 파싱을 위한 특별한 함수나 라이브러리가 내장되어 있습니다. 예를 들어, Python에는 strptime이라는 함수가, Java에는 SimpleDateFormat 클래스가 있습니다.

Lua의 match 함수는 이러한 문제에 대한 간단하고 효율적인 해결책입니다. 이 함수는 주어진 패턴에 맞는 문자를 반환하여 문자열에서 원하는 정보를 쉽게 추출할 수 있게 합니다.

## 참고 자료

- Lua에 대한 자세한 내용은 [The Programming Language Lua](http://www.lua.org/)를 참고하세요.
- Lua로 날짜와 시간을 다루는 더 많은 방법은 [Lua-users wiki: Dates and Time](http://lua-users.org/wiki/DatesAndTime)를 참조하세요.
- 다른 프로그래밍 언어에서 날짜 파싱의 예제와 비교해보려면 [Python's strptime](https://docs.python.org/3/library/datetime.html#strftime-strptime-behavior)과 [Java's SimpleDateFormat](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)를 참고하세요.