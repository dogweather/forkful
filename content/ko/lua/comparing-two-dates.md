---
title:                "두 날짜를 비교하는 방법"
html_title:           "Lua: 두 날짜를 비교하는 방법"
simple_title:         "두 날짜를 비교하는 방법"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

비교하기로 하는 두 날짜가 무엇인지와 왜 프로그래머들이 이 작업을 하는지 두 가지를 설명합니다.

## 어떻게:

```lua
-- 날짜 1
local date1 = os.time({day = 10, month = 10, year = 2020})

-- 날짜 2
local date2 = os.time({day = 15, month = 10, year = 2020})

-- 두 날짜를 비교하여 차이를 계산
local diff = os.difftime(date2, date1)

print(diff) --> 432000 (초 단위로 차이가 출력됨)
```

## 깊이 알아보기:

* **역사적 맥락:** 두 날짜를 비교하는 기능은 컴퓨터 시스템의 시간 관리에 필수적이었습니다. 예를 들어, 파일 생성/변경 시간을 비교하거나 작업 로그를 분석하는 데 사용될 수 있습니다.

* **대안:** 루아에서는 일반적으로 `os.difftime` 함수를 사용하여 두 날짜를 비교합니다. 하지만 다른 프로그래밍 언어나 라이브러리에서는 다른 방법을 사용할 수도 있습니다.

* **구현 세부 사항:** 루아에서는 날짜를 비교하기 위해 `os.time`과 `os.difftime` 함수를 사용합니다. 이들 함수는 시스템의 현재 시간 정보를 활용하여 계산됩니다. 

## 관련 자료:

* [루아 공식 문서: 날짜/시간 관련 함수](https://www.lua.org/pil/22.html)
* [다른 프로그래밍 언어에서의 날짜 비교 방법 설명](https://stackoverflow.com/questions/1486739/how-to-compare-two-dates)