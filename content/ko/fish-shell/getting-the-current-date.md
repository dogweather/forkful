---
title:                "현재 날짜 가져오기"
html_title:           "C: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

현재 날짜를 얻는 것은 프로그램이 사용자에게 현재 시간 정보를 제공하도록 하거나 일부 기능이 시간에 따라 다르게 동작하게 하는 등의 기능을 구현하기 위해 사용됩니다. 이는 로깅, 이벤트 추적, 파일 타임스탬프 생성 등에 활용 될 수 있습니다. 

## 어떻게:

Fish Shell에서 현재 날짜를 얻는 기본적인 방법은 다음과 같습니다:

```Fish Shell
date
```

이 명령을 실행하면 현재 날짜와 시간이 출력됩니다. 예를 들어:

```Fish Shell
화 2023년 06월 20일 20:20:20 KST 2023
```

## 디테일하게:

Fish Shell은 2005년 처음 출시되어 시간의 흐름에 따라 많은 업데이트를 거쳤습니다. 오늘날 `date` 명령은 POSIX 표준에 기반하여 작동하며 거의 모든 유닉스 기반 시스템에서 널리 지원됩니다.

현재 날짜를 얻는 다른 방법으로는 에폭 시간(epoch time)인 `time` 명령어를 사용하여 현재 시간을 초 단위로 표시하는 방법이 있습니다. 이러한 방법은 각각의 상황에 따라 적절하게 활용 될 수 있습니다.

```Fish Shell
time
```

또한, `date` 명령은 서식 지정자를 포함하여 많은 수정 가능한 옵션들을 제공합니다. 이를 통해 원하는 형식으로 날짜와 시간을 출력할 수 있습니다.

```Fish Shell
date "+%Y-%m-%d"
```

위의 코드는 현재 날짜를 'YYYY-MM-DD' 형식으로 출력합니다.

## 참고 링크:

* [Fish Shell 공식 문서](https://fishshell.com/docs/current/index.html)
* [GNU Date Manual](https://www.gnu.org/software/coreutils/manual/coreutils.html#Date-input-formats)
* [Unix Time 에폭 및 변환](https://www.epochconverter.com/)
* [POSIX 표준](https://pubs.opengroup.org/onlinepubs/9699919799/)