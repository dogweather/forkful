---
date: 2024-01-20 17:36:42.993690-07:00
description: "How to (\uBC29\uBC95) \uCC98\uC74C\uC5D0\uB294 \uB9AC\uB205\uC2A4\uC640\
  \ \uC720\uB2C9\uC2A4 \uC2DC\uC2A4\uD15C\uC5D0\uC11C \uC2DC\uAC04\uACFC \uB0A0\uC9DC\
  \uB97C \uB2E4\uB8E8\uAE30 \uC704\uD55C \uBA85\uB839\uC5B4\uB85C 'date' \uBA85\uB839\
  \uC5B4\uAC00 \uC788\uC5C8\uC2B5\uB2C8\uB2E4. 'date' \uBA85\uB839\uC5B4\uB294 \uB0A0\
  \uC9DC\uB97C \uC124\uC815\uD558\uAC70\uB098 \uD45C\uC2DC\uD558\uAE30 \uC704\uD574\
  \ \uC0AC\uC6A9\uB429\uB2C8\uB2E4. \uBB38\uC790\uC5F4\uB85C\uC758 \uBCC0\uD658 \uAE30\
  \uB2A5\uC740 \uC774\uB7F0 \uBA85\uB839\uC5B4\uC758 \uC720\uC5F0\uC131\uC744 \uBCF4\
  \uC5EC\uC90D\uB2C8\uB2E4. \uB2E4\uB978 \uBC29\uBC95\uC73C\uB85C\uB294\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.167292-06:00'
model: gpt-4-1106-preview
summary: "'date' \uBA85\uB839\uC5B4\uB294 \uB0A0\uC9DC\uB97C \uC124\uC815\uD558\uAC70\
  \uB098 \uD45C\uC2DC\uD558\uAE30 \uC704\uD574 \uC0AC\uC6A9\uB429\uB2C8\uB2E4."
title: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD558\uAE30"
weight: 28
---

## How to (방법)
```Bash
# 현재 날짜를 YYYY-mm-dd 형식의 문자열로 변환
date_str=$(date +"%Y-%m-%d")
echo $date_str
```
출력 예시:
```
2023-04-02
```

```Bash
# 사용자 지정 날짜를 문자열로 변환 (예: 2023년 5월 1일)
date_str=$(date -d '2023-05-01' +"%Y년 %m월 %d일")
echo $date_str
```
출력 예시:
```
2023년 05월 01일
```

## Deep Dive (심층 분석)
처음에는 리눅스와 유닉스 시스템에서 시간과 날짜를 다루기 위한 명령어로 'date' 명령어가 있었습니다. 'date' 명령어는 날짜를 설정하거나 표시하기 위해 사용됩니다. 문자열로의 변환 기능은 이런 명령어의 유연성을 보여줍니다.

다른 방법으로는 'strftime'이라는 함수를 사용할 수 있는데, 다양한 프로그래밍 언어에서 지원하는 함수로 날짜와 시간을 원하는 형식의 문자열로 변환할 수 있습니다. Bash에서는 'date' 명령어를 통해서 'strftime' 기능을 사용합니다.

리눅스 환경에서는 'date' 명령어가 널리 쓰이고 있으며, 그 구현은 GNU Coreutils 패키지에 포함되어 있습니다. 이 명령어는 시스템의 시간대 설정에 따라 출력이 달라질 수 있기 때문에, 스크립트가 다양한 환경에서 실행될 때는 시간대를 고려해야 합니다.

## See Also (참고 자료)
- GNU Coreutils 'date' 매뉴얼: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Bash 날짜와 시간 처리: https://www.tldp.org/LDP/abs/html/timedate.html
- 언어별 'strftime' 포맷팅: http://strftime.org/
