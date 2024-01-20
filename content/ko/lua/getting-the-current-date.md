---
title:                "현재 날짜 가져오기"
html_title:           "C: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 무엇이며 왜합니까?

현재 날짜를 받아오는 것은 시스템의 현재 날짜와 시간을 알아내는 프로그래밍 작업입니다. 이는 로그 작성, 이벤트 추적, 날짜와 시간 기반의 기능 등에서 필요로 합니다.

## 어떻게:

Lua에서는 os.date() 함수를 이용하여 현재 날짜와 시간을 얻을 수 있습니다.

```Lua
date = os.date("*t")
print("오늘은 "..date.year.."년 "..date.month.."월 "..date.day.."일 입니다.")
```
위 코드의 예제 출력:

```
오늘은 2021년 12월 25일 입니다.
```

## 깊게 살펴보기:

Lua에서 날짜와 시간을 다루는 것은 os.date()와 os.time() 함수에 의해 다루어집니다. 이러한 함수들은 ANSI C에서 정의된 것을 바탕으로 합니다. 또한 Lua에서는 이 외에도 os.difftime() 함수를 이용해 두 시간 사이의 차이를 측정하는 기능도 제공합니다.
 
대안적으로, 더 복잡한 날짜와 시간 연산이 필요한 경우에는 'luadate'와 같은 외부 라이브러리를 사용할 수도 있습니다.

## 참조하기:

1. Lua 공식 문서 - os.date : https://www.lua.org/manual/5.2/manual.html#pdf-os.date
2. Lua 공식 문서 - os.time : https://www.lua.org/manual/5.2/manual.html#pdf-os.time
3. Lua 외부 라이브러리 luadate : https://github.com/Tieske/date