---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:58.513182-07:00
description: "Bash\uC5D0\uC11C \uD604\uC7AC \uB0A0\uC9DC\uB97C \uAC80\uC0C9\uD558\uB294\
  \ \uAC83\uC740 \uB2E4\uC591\uD55C \uD615\uC2DD\uC73C\uB85C \uB0A0\uC9DC\uC640 \uC2DC\
  \uAC04\uC744 \uD45C\uC2DC\uD558\uB294 \uB0B4\uC7A5 \uBA85\uB839\uC744 \uC0AC\uC6A9\
  \uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uC774 \uAE30\uB2A5\uC744 \uB85C\uADF8 \uD0C0\uC784\uC2A4\uD0EC\
  \uD551, \uC791\uC5C5 \uC608\uC57D \uB610\uB294 \uC218\uD589\uB41C \uB3D9\uC791\uC744\
  \ \uCD94\uC801\uD558\uAE30 \uC704\uD574 \uC2DC\uC2A4\uD15C \uC815\uBCF4 \uC2A4\uD06C\
  \uB9BD\uD2B8\uC758 \uC77C\uBD80\uB85C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.497289-06:00'
model: gpt-4-0125-preview
summary: "Bash\uC5D0\uC11C \uD604\uC7AC \uB0A0\uC9DC\uB97C \uAC80\uC0C9\uD558\uB294\
  \ \uAC83\uC740 \uB2E4\uC591\uD55C \uD615\uC2DD\uC73C\uB85C \uB0A0\uC9DC\uC640 \uC2DC\
  \uAC04\uC744 \uD45C\uC2DC\uD558\uB294 \uB0B4\uC7A5 \uBA85\uB839\uC744 \uC0AC\uC6A9\
  \uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4."
title: "\uD604\uC7AC \uB0A0\uC9DC \uAC00\uC838\uC624\uAE30"
weight: 29
---

## 방법:
Bash에서 현재 날짜와 시간을 얻기 위한 주요 도구는 `date` 명령입니다. 다음은 이를 사용하는 몇 가지 예시입니다:

1. **기본 형식으로 현재 날짜와 시간 얻기:**

```bash
date
```

*출력 예시:*
```
Wed Apr 5 14:22:04 PDT 2023
```

2. **출력 형식 사용자 정의:** 출력 형식을 지정하려면 `+%` 형식 지정자를 사용할 수 있습니다. 예를 들어, YYYY-MM-DD 형식으로 날짜를 표시하려면:

```bash
date "+%Y-%m-%d"
```

*출력 예시:*
```
2023-04-05
```

3. **현재 UNIX 타임스탬프 얻기:** UNIX 타임스탬프는 Unix Epoch(1970년 1월 1일) 이후의 초 수입니다. 이는 시간 차이를 기반으로 계산을 수행하는 스크립트에 유용합니다.

```bash
date "+%s"
```

*출력 예시:*
```
1672877344
```

Bash에서 이 기본 작업에 대해 일반적으로 사용되는 인기 있는 제3자 라이브러리는 없습니다. 왜냐하면 내장된 `date` 명령이 포괄적인 기능을 제공하기 때문입니다. 그러나 보다 고급 날짜 및 시간 조작을 위해, 프로그래머들은 날짜 산술 및 파싱을 위한 라이브러리를 제공하는 다른 프로그래밍 언어나 도구, 예를 들어 Python의 `datetime` 모듈을 사용할 수 있습니다.
