---
title:                "미래나 과거의 날짜 계산하기"
aliases:
- /ko/fish-shell/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:30:52.216481-07:00
model:                 gpt-4-1106-preview
simple_title:         "미래나 과거의 날짜 계산하기"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why? (무엇 그리고 왜?)
날짜 계산은 특정 일자로부터 미래나 과거의 날짜를 찾는 것입니다. 프로그래머들은 일정 관리, 기간 계산, 기념일 추적 등의 작업을 자동화하기 위해 이를 수행합니다.

## How to: (어떻게 하나요?)
Fish Shell에서 날짜를 계산하려면 `date` 명령어와 함께 사용합니다. 예제를 보시죠.

미래 날짜 계산하기 (10일 후):
```Fish Shell
set -l future_date (date -d "+10 days" +"%Y-%m-%d")
echo $future_date
```
출력 예시:
```
2023-09-17
```

과거 날짜 계산하기 (30일 전):
```Fish Shell
set -l past_date (date -d "-30 days" +"%Y-%m-%d")
echo $past_date
```
출력 예시:
```
2023-08-18
```

## Deep Dive (심도 있는 분석)
`date` 명령어는 유닉스 시스템에서 오래 전부터 이용되었습니다. Fish Shell은 이전 명령어와 호환성을 유지하면서 더 간결하고 직관적인 구문을 제공합니다.

대안으로 `dateutils.dadd` 같은 도구들이 있지만, Fish Shell내에서 바로 `date`를 사용하는 것이 더 편리합니다. 구현 세부사항으로, 날짜 계산 시 시간대를 고려해야 할 수 있으니, `date` 명령의 `-u` (UTC) 옵션을 사용하는 것을 고려하세요.

## See Also (참고 자료)
- Fish Shell Documentation: https://fishshell.com/docs/current/index.html
- `date` 명령어 매뉴얼: `man date`
- DateUtils: http://www.fresse.org/dateutils/
