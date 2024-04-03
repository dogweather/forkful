---
date: 2024-01-20 17:30:52.216481-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) Fish Shell\uC5D0\uC11C\
  \ \uB0A0\uC9DC\uB97C \uACC4\uC0B0\uD558\uB824\uBA74 `date` \uBA85\uB839\uC5B4\uC640\
  \ \uD568\uAED8 \uC0AC\uC6A9\uD569\uB2C8\uB2E4. \uC608\uC81C\uB97C \uBCF4\uC2DC\uC8E0\
  . \uBBF8\uB798 \uB0A0\uC9DC \uACC4\uC0B0\uD558\uAE30 (10\uC77C \uD6C4)."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.874977-06:00'
model: gpt-4-1106-preview
summary: "Fish Shell\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uACC4\uC0B0\uD558\uB824\uBA74\
  \ `date` \uBA85\uB839\uC5B4\uC640 \uD568\uAED8 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uBBF8\uB798\uB098 \uACFC\uAC70\uC758 \uB0A0\uC9DC \uACC4\uC0B0\uD558\uAE30"
weight: 26
---

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
