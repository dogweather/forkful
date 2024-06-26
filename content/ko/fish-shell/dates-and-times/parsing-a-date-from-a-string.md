---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:19.754490-07:00
description: "\uBC29\uBC95: Fish Shell\uC5D0\uC11C\uB294 \uBB38\uC790\uC5F4\uC5D0\uC11C\
  \ \uB0A0\uC9DC\uB97C \uD30C\uC2F1\uD558\uAE30 \uC704\uD574 \uD2B9\uBCC4\uD788 \uC124\
  \uACC4\uB41C \uB0B4\uC7A5 \uBA85\uB839\uC5B4\uAC00 \uC5C6\uC2B5\uB2C8\uB2E4. \uB300\
  \uC2E0 `date`(Linux \uBC0F macOS\uC5D0\uC11C \uC0AC\uC6A9 \uAC00\uB2A5)\uC640 \uAC19\
  \uC740 \uC678\uBD80 \uC720\uD2F8\uB9AC\uD2F0\uC5D0 \uC758\uC874\uD558\uAC70\uB098\
  \ `GNU date`\uC640 \uAC19\uC740 \uC778\uAE30 \uC788\uB294 \uD0C0\uC0AC \uB3C4\uAD6C\
  \uB97C \uC774\uC6A9\uD558\uC5EC \uBCF4\uB2E4 \uBCF5\uC7A1\uD55C \uD30C\uC2F1\uC744\
  \u2026"
lastmod: '2024-03-13T22:44:55.869856-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell\uC5D0\uC11C\uB294 \uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC\
  \uB97C \uD30C\uC2F1\uD558\uAE30 \uC704\uD574 \uD2B9\uBCC4\uD788 \uC124\uACC4\uB41C\
  \ \uB0B4\uC7A5 \uBA85\uB839\uC5B4\uAC00 \uC5C6\uC2B5\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC \uBD84\uC11D\uD558\uAE30"
weight: 30
---

## 방법:
Fish Shell에서는 문자열에서 날짜를 파싱하기 위해 특별히 설계된 내장 명령어가 없습니다. 대신 `date`(Linux 및 macOS에서 사용 가능)와 같은 외부 유틸리티에 의존하거나 `GNU date`와 같은 인기 있는 타사 도구를 이용하여 보다 복잡한 파싱을 수행합니다. 접근 방법은 다음과 같습니다:

**Fish에서 `date` 사용하기:**

"YYYY-MM-DD" 형식의 날짜 문자열을 파싱하려면, 문자열 뒤에 `-d`(또는 GNU date의 경우 `--date`) 옵션을 사용한 `date` 명령을 사용할 수 있습니다. `+` 옵션은 출력 형식을 지정하는 데 사용됩니다.

```fish
set date_str "2023-04-01"
date -d $date_str +"%A, %d %B %Y"
# 출력: 토요일, 01 4월 2023
```

macOS의 경우(`-j` 및 `-f` 플래그에 대한 다른 형식이 필요함):

```fish
set date_str "2023-04-01"
date -j -f "%Y-%m-%d" $date_str +"%A, %d %B %Y"
# 출력: 토요일, 01 4월 2023
```

**복잡한 파싱을 위한 GNU `date` 사용하기:**

GNU `date`는 문자열 형식에 대해 더 유연합니다. 명시적으로 입력 형식을 지정하지 않고도 많은 일반적인 날짜 문자열 형식을 자동으로 감지할 수 있습니다:

```fish
set complex_date_str "April 1, 2023 14:00"
date -d "$complex_date_str" '+%Y-%m-%d %H:%M:%S'
# 출력: 2023-04-01 14:00:00
```

그러나 자동으로 인식되지 않을 수 있는 날짜 문자열을 사용하거나 입력 형식을 정확하게 제어할 필요가 있을 때는 GNU `date`에서 입력 형식을 지정하는 것이 직접적으로 지원되지 않습니다. 이러한 경우에는 문자열을 사전 처리하거나 보다 복잡한 날짜 파싱 루틴을 위해 설계된 다른 도구를 사용하는 것을 고려하십시오.
