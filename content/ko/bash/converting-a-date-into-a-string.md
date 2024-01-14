---
title:                "Bash: 날짜를 문자열로 변환하기"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 변환하는 과정은 Bash 프로그래밍에서 매우 중요합니다. 날짜는 다양한 형식으로 사용되며 문자열로 변환하는 것은 날짜를 포맷팅하여 더 쉽게 사용할 수 있도록 해줍니다.

## 어떻게

다음은 날짜를 문자열로 변환하는 간단한 Bash 스크립트 예제입니다.

```Bash
#!/bin/bash

# 현재 날짜를 YYYY-MM-DD 형식으로 문자열로 변환
current_date=$(date "+%Y-%m-%d")

echo "오늘 날짜는 $current_date 입니다."
```

위 스크립트를 실행하면 다음과 같은 출력을 볼 수 있습니다.

```
오늘 날짜는 2021-10-15 입니다.
```

날짜를 포맷팅하는데에는 `date` 명령어의 `%` 문자와 특정 문자를 조합하여 사용합니다. 이를 이용하여 원하는 형식으로 날짜를 문자열로 변환할 수 있습니다. 아래는 자주 사용되는 날짜 포맷문자들입니다.

- `%Y`: 연도 (예: 2021)
- `%m`: 월 (예: 10)
- `%d`: 일 (예: 15)
- `%H`: 24시간 형식의 시간 (예: 10)
- `%M`: 분 (예: 30)
- `%S`: 초 (예: 45)
- `%a`: 요일의 축약형 (예: Mon)
- `%A`: 요일의 전체 이름 (예: Monday)
- `%b`: 월의 축약형 (예: Jan)
- `%B`: 월의 전체 이름 (예: January)

더 많은 날짜 포맷문자들은 `man date` 명령어를 통해 확인할 수 있습니다.

## 딥 다이브

날짜 포맷팅에서 더 깊이 들어가 볼 수 있는 주제는 다양합니다. 예를 들어, 현재 시간을 기준으로 몇 시간 전/후의 날짜를 알아내는 방법이나 특정 날짜의 요일을 구하는 방법 등이 있습니다. 또한, 다양한 시간대를 고려해 날짜를 포맷팅하는 방법도 실무에서 유용하게 사용될 수 있습니다.

## 참고 자료
- [Bash 날짜 포맷팅 관련 문서](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html#Shell-Parameter-Expansion)
- [Bash에서 날짜 구하기](https://www.shellhacks.com/get-current-date-time-bash-script-linux/)
- [Bash 입문자를 위한 기본 명령어 모음](https://www.learnshell.org/)
- [Bash 프로그래밍 기초](https://wiki.kldp.org/KoreanDoc/html/NewbieGuide/html/TheLinuxCommand_BashShell.html#SECT_3_4)
- [Bash 스크립트 관련 자주 묻는 질문들](https://tldp.org/LDP/Bash-Beginners-Guide/html/sect_02_01.html)

## 참고 자료

- [날짜 포맷팅 관련 문서](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html#Shell-Parameter-Expansion)
- [Bash에서 날짜 구하기](https://www.shellhacks.com/get-current-date-time-bash-script-linux/)
- [Bash 입문자를 위한 기본 명령어 모음](https://www.learnshell.org/)
- [Bash 프로그래밍 기초](https://wiki.kldp.org/KoreanDoc/html/NewbieGuide/html/TheLinuxCommand_BashShell.html