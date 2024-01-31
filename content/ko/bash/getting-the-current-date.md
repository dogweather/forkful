---
title:                "현재 날짜 가져오기"
date:                  2024-01-20T15:13:11.366999-07:00
html_title:           "Bash: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"

category:             "Bash"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
현재 날짜 가져오기는 시스템의 현재 날짜와 시간을 찾는 것입니다. 로그 파일 작성, 백업 스크립트 실행 또는 타임스탬프 생성 등의 목적으로 프로그래머들이 사용합니다.

## How to: (방법)
```Bash
# 현재 날짜와 시간 출력
date

# 형식을 지정하여 날짜 출력
date +"%Y-%m-%d %H:%M:%S"

# 예제 출력
2023-04-01 12:34:56
```

## Deep Dive (심층 분석)
쉘 프로그래밍에서 날짜는 중요한 역할을 합니다. `date` 명령은 UNIX 시스템에서 시작되어 리눅스 배포판까지 이어져 왔습니다. 대체 방법으로는 `printf` 내장 명령을 사용하거나 Perl이나 Python과 같은 스크립팅 언어를 활용할 수 있습니다. `date`의 활용은 스크립트 효율성과 가독성을 향상시킵니다. 단순한 명령이지만 시스템의 타임존 설정이나 여름시간제(Daylight Saving Time) 같은 요소에 영향을 받습니다.

## See Also (참고 자료)
- GNU Coreutils `date`: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Bash Scripting Guide: https://www.tldp.org/LDP/abs/html/
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/datetime.html
