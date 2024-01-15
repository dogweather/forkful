---
title:                "현재 날짜 가져오기"
html_title:           "Bash: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜

현재 날짜를 가져오는 것의 이점은 사람들이 그날의 계획을 세울 수 있고, 일정을 조정할 수 있으며, 파일을 날짜별로 정리할 수 있기 때문입니다.

## 방법

다음은 Bash에서 현재 날짜를 가져오는 간단한 코드입니다.

```Bash
date
```

이 코드를 실행하면 현재 날짜와 시간이 출력됩니다.

```Bash
Tue Jul 27 14:00:00 KST 2021
```

만약 날짜를 원하는 형식으로 나타내고 싶다면 `-u` 옵션과 출력 형식을 지정하는 `+`를 사용할 수 있습니다. 다음은 ISO 8601 형식으로 날짜를 나타내는 예시 코드입니다.

```Bash
date -u +"%Y-%m-%dT%H:%M:%SZ"
```

위 코드를 실행하면 다음과 같은 출력이 나타납니다.

```Bash
2021-07-27T05:00:00Z
```

## 더 깊게 들어가기

`date` 명령어는 컴퓨터의 레지스트리에 저장된 시간 값을 확인하고 이를 보여줍니다. 이를 통해 사용자는 컴퓨터가 어떤 시간 정보를 파악하고 처리하는지 알 수 있습니다.

## 관련 링크

- [Bash 공식 문서](https://www.gnu.org/software/bash/manual/)
- [Bash 프로그래밍 실습](https://www.learnshell.org/)
- [Linux 커맨드 라인 설명서](https://www.tldp.org/LDP/abs/html/)