---
title:                "문자열에서 날짜 분석하기"
aliases:
- /ko/fish-shell/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:14:19.754490-07:00
model:                 gpt-4-0125-preview
simple_title:         "문자열에서 날짜 분석하기"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열에서 날짜를 파싱하는 것은 문자열 내에 인코딩된 날짜 정보를 추출하고 프로그래밍 환경에서 인식하고 조작할 수 있는 구조화된 형식으로 변환하는 작업을 포함합니다. 프로그래머는 날짜 비교, 연산, 포맷팅 및 지역화와 같은 작업을 가능하게 하여 소프트웨어에서 스케줄링, 타임스탬프 및 역사적 데이터를 효율적으로 처리할 수 있도록 이 작업을 수행합니다.

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
