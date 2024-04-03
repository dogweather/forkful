---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:52.274845-07:00
description: "Bash\uC5D0\uC11C \uBB38\uC790\uC5F4\uB85C\uBD80\uD130 \uB0A0\uC9DC\uB97C\
  \ \uD30C\uC2F1\uD558\uB294 \uAC83\uC740 \uBB38\uC790 \uB370\uC774\uD130\uB85C\uBD80\
  \uD130 \uB0A0\uC9DC \uC815\uBCF4\uB97C \uCD94\uCD9C\uD558\uC5EC Bash\uC5D0\uC11C\
  \ \uC870\uC791\uD558\uAC70\uB098 \uCD94\uAC00\uC801\uC778 \uD504\uB85C\uC138\uC2A4\
  \uC5D0 \uC0AC\uC6A9\uD560 \uC218 \uC788\uB294 \uD615\uC2DD\uC73C\uB85C \uBCC0\uD658\
  \uD558\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uC774\uB294 \uB85C\uADF8\
  \ \uD30C\uC77C \uBD84\uC11D, \uB0A0\uC9DC \uC2A4\uD0EC\uD504\uB97C \uAE30\uBC18\uC73C\
  \uB85C \uD55C \uD30C\uC77C \uC870\uC9C1\uD654, \uC790\uB3D9 \uBCF4\uACE0\uC11C \uC0DD\
  \uC131\uACFC \uAC19\uC740\u2026"
lastmod: '2024-03-13T22:44:55.495667-06:00'
model: gpt-4-0125-preview
summary: "Bash\uC5D0\uC11C \uBB38\uC790\uC5F4\uB85C\uBD80\uD130 \uB0A0\uC9DC\uB97C\
  \ \uD30C\uC2F1\uD558\uB294 \uAC83\uC740 \uBB38\uC790 \uB370\uC774\uD130\uB85C\uBD80\
  \uD130 \uB0A0\uC9DC \uC815\uBCF4\uB97C \uCD94\uCD9C\uD558\uC5EC Bash\uC5D0\uC11C\
  \ \uC870\uC791\uD558\uAC70\uB098 \uCD94\uAC00\uC801\uC778 \uD504\uB85C\uC138\uC2A4\
  \uC5D0 \uC0AC\uC6A9\uD560 \uC218 \uC788\uB294 \uD615\uC2DD\uC73C\uB85C \uBCC0\uD658\
  \uD558\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC \uBD84\uC11D\uD558\uAE30"
weight: 30
---

## 방법:
Bash 자체는 날짜 파싱 기능이 상당히 제한적이며, `date` 및 `awk`와 같은 외부 도구에 종종 의존하여 보다 섬세한 조작을 수행합니다. 특정 형식을 파싱한 다음 `date` 명령어를 사용하여 변환하거나 작업을 수행하는 방법은 다음과 같습니다.

**예시 1:** 날짜 문자열을 추출하여 다른 형식으로 변환하기.

`yyyy-mm-dd` 형식의 날짜가 있고 이를 `dd-mm-yyyy`로 변환하려는 경우가 있다고 가정합니다.

```bash
original_date="2023-04-01"
formatted_date=$(date -d $original_date '+%d-%m-%Y')

echo $formatted_date
```

**출력 예시:**
```
01-04-2023
```

이는 `date` 명령어와 `-d` 옵션을 사용하여 입력 날짜 문자열을 지정하고, `+%d-%m-%Y`를 사용하여 출력 형식을 지정합니다.

**예시 2:** `awk`를 사용하여 구조화된 텍스트 라인에서 날짜를 파싱하고 변환하기.

다음과 같은 로그 파일 라인이 있다고 가정합니다:

```
2023-04-01 12:00:00 사용자가 로그인했습니다
```

`awk`와 `date`를 사용하여 날짜 부분을 추출하고 변환할 수 있습니다.

```bash
log_line="2023-04-01 12:00:00 사용자가 로그인했습니다"
date_part=$(echo $log_line | awk '{print $1}')
formatted_date=$(date -d $date_part "+%A, %B %d, %Y")

echo $formatted_date
```

**출력 예시:**
```
토요일, 4월 01, 2023
```

이 예시에서는 `awk`를 사용하여 로그 라인을 분할하고 날짜 부분을 추출합니다(`$1`은 첫 번째 공백으로 구분된 필드를 나타냅니다). 그 다음 `date`로 다시 형식을 지정합니다.

### 타사 도구 사용하기
보다 복잡한 파싱이 필요하거나 다양한 날짜 형식을 다룰 때, `dateutils`와 같은 타사 도구가 매우 유용할 수 있습니다.

**`dateutils`를 사용한 예시:**

예를 들어, 비표준 형식인 `4월 01, 2023`의 날짜 문자열이 있다고 가정합니다.

```bash
original_date="4월 01, 2023"
formatted_date=$(dateconv -i "%B %d, %Y" -f "%Y-%m-%d" <<< $original_date)

echo $formatted_date
```

**출력 예시:**
```
2023-04-01
```

이 명령어는 `dateutils`의 `dateconv`를 사용하여 `-i`로 입력 형식을, `-f`로 원하는 출력 형식을 지정합니다. `dateutils`는 광범위한 날짜 및 시간 형식을 지원하여 Bash 스크립트에서의 날짜 파싱 작업에 매우 다양하게 사용될 수 있습니다.
