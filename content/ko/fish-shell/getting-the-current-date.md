---
title:                "현재 날짜 가져오기"
date:                  2024-01-20T15:14:39.201720-07:00
html_title:           "Bash: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
현재 날짜를 얻는 것은 시스템의 현재 날짜와 시간을 검색하는 과정입니다. 프로그래머들은 로깅, 타임스탬프 생성, 사용자 인터페이스 업데이트 등 다양한 이유로 이를 사용합니다.

## How to: (어떻게 하나요?)
Fish Shell에서 현재 날짜와 시간을 얻으려면 `date` 명령어를 사용하세요.

```Fish Shell
set current_date (date)
echo $current_date
```

Sample output:

```
Wed Mar 31 10:05:21 KST 2023
```

단지 날짜만 또는 시간만 필요하다면, `date` 명령어에 `+%Y-%m-%d` 또는 `+%H:%M:%S` 포맷을 사용하세요.

```Fish Shell
# 날짜만 가져오기
set just_date (date "+%Y-%m-%d")
echo $just_date

# 시간만 가져오기
set just_time (date "+%H:%M:%S")
echo $just_time
```

Sample output:

```
2023-03-31
10:06:42
```

## Deep Dive (심층 탐구)
`date` 명령어는 UNIX 시스템에서 오래전부터 사용되어 온 일반적인 도구입니다. 이는 시간 관련 계산과 조작을 위한 강력한 기능을 제공합니다. Fish Shell에서 `date`는 외부 명령어이며, 시스템의 /bin/date 또는 /usr/bin/date에 연결됩니다. POSIX 표준을 따르므로 대부분의 유닉스 계열 시스템에서 비슷하게 동작합니다.

옵션 `-u`를 사용하면 협정 세계시(UTC)가 반환되므로 시간대가 다른 시스템 간 일관된 타임스탬프를 생성할 수 있습니다.

```Fish Shell
# UTC 시간 가져오기
set utc_time (date -u)
echo $utc_time
```

Fish Shell은 Bash Shell과 같은 다른 쉘과 비교해 봤을 때, 보다 읽기 쉽고 사용하기 간편한 구문을 제공합니다. 그러나 날짜와 시간을 사용할 때는 기본적으로 내부 명령어 대신 외부 `date` 명령어에 의존합니다.

## See Also (참고 자료)
- Fish Shell 공식 문서: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- `date` 명령어 매뉴얼 페이지: `man date` in terminal or [https://man7.org/linux/man-pages/man1/date.1.html](https://man7.org/linux/man-pages/man1/date.1.html)
- POSIX 표준에 관한 정보: [https://pubs.opengroup.org/onlinepubs/9699919799/utilities/date.html](https://pubs.opengroup.org/onlinepubs/9699919799/utilities/date.html)
