---
title:                "현재 날짜 가져오기"
html_title:           "C: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하나요?

현재 날짜를 얻는 것은 복잡하지 않은데, 현재의 시간 번위를 코드에 반영하기 위한 방법입니다. 이를 통해 애플리케이션 로그에 타임스탬프를 붙이거나 예정된 작업을 및 그 외 다양한 상황에서 유용하게 사용할 수 있습니다.

## 어떻게 사용하나요:

아래의 코드를 쓰면 현재 날짜를 얻을 수 있습니다:

```Bash
date
```

이 코드를 실행하면 아래와 같이 출력됩니다:

```Bash
Tue Sep 14 15:30:29 KST 2021
```

다양한 패턴으로 날짜와 시간을 표시할 수 있습니다:

```Bash
date '+%Y-%m-%d'
```

위의 코드를 실행하면 아래와 같이 출력됩니다:

```Bash
2021-09-14
```

## 깊이 보기

Unix 시스템의 첫 번째 버전에서부터 `date` 커맨드는 현재 날짜 및 시간을 얻어 사용자에게 표시하는 기본 도구였습니다. `date` 커맨드에 다양한 포맷 옵션을 전달함으로써 원하는 날짜와 시간 패턴을 출력할 수 있습니다.

대안으로, 날짜와 시간 정보를 얻기 위해 쉘 스크립트에서 Perl 또는 Python 등의 다른 스크립트 언어를 사용할 수도 있습니다. 

## 참고자료:

- Bash Manual: https://www.gnu.org/software/bash/manual/bash.html
- Date Command in Linux with examples: https://www.geeksforgeeks.org/date-command-linux-examples/