---
title:                "날짜를 문자열로 변환하기"
html_title:           "Arduino: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

날짜를 문자열로 변환하는 것은 특정 날짜를 사람이 읽을 수 있는 형식 (예: "2022년 2월 22일")으로 변경하는 것을 말합니다. 프로그래머가 이 작업을 하는 이유는 일반적으로 사용자 친화적인 출력을 제공하거나 텍스트 기반의 저장 또는 전송 매체를 위해 날짜를 준비하기 위함입니다.

## 방법:

Lua에서 날짜를 문자열로 변환하는 가장 간단한 방법은 `os.date` 함수를 사용하는 것입니다. 아래 코드와 출력을 살펴 보겠습니다.

```Lua
print(os.date("%Y년 %m월 %d일"))
```
현재 시스템의 날짜가 2022년 2월 22일이라면 출력 결과는 다음과 같을 것입니다.
```Lua
2022년 02월 22일
```

## 깊게 알아보기:

Lua에서 날짜를 문자열로 변환하는 작업은 Lua 3.2에서 처음 도입된 `os.date` 함수를 이용하여 수행할 수 있습니다. 이 함수는 매우 유연하며 문자열 형식을 제어하는 다양한 지시자들을 허용합니다.

날짜를 문자열로 변환할 때의 주된 대안은 대개 `os.time` 사용이며, 이 함수는 시간을 유닉스 타임스탬프(1970년 1월 1일 이후 초 단위로 된 시간)로 반환합니다. 이를 문자열로 변환하여 저장하거나 전송할 수 있지만, 숫자 형태이므로 사람이 읽기 힘들 수 있습니다.

`os.date`의 구현은 C 표준 라이브러리의 `strftime` 함수에 기반을 두고 있습니다. 이는 입력 형식 지시자를 받아 이에 대응하는 문자열로 반환하는 방식으로 동작합니다.

## 참고하기:

1. Lua에서 날짜와 시간 다루기: https://www.tutorialspoint.com/lua/lua_date_time.htm
2. Lua `os.date` 함수 문서: https://www.lua.org/manual/5.2/manual.html#6.22
3. C 표준 라이브러리의 `strftime` 함수에 대한 문서: http://www.cplusplus.com/reference/ctime/strftime/