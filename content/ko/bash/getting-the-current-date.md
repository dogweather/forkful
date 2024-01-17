---
title:                "현재 날짜 얻기"
html_title:           "Bash: 현재 날짜 얻기"
simple_title:         "현재 날짜 얻기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 현재 날짜란?
현재 날짜를 알아내는 것은 우리가 현재 시각을 알고 싶을 때 자주 하는 작업입니다. 프로그래머들은 이를 사용하여 다양한 목적에 활용할 수 있습니다.

## 어떻게 할까요?
```Bash
date +%Y%m%d
```
```Bash
20210527
```
위의 예시 코드를 Bash 프로그램으로 실행하면 현재 날짜를 년, 월, 일 순서로 출력합니다.

## 깊이 파고들기
현재 날짜를 알아내기 위해서는 시스템의 시간 및 날짜 정보를 읽어오는 명령어인 "date"를 사용합니다. 이 명령어는 Unix-like 시스템에서 많이 사용되며, GNU Coreutils 프로젝트에 포함되어 있습니다.

대부분의 운영 체제에서 date 명령어는 다양한 포맷 옵션을 지원하며, 우리가 원하는 형식으로 날짜를 출력할 수 있습니다.

또한 현재 날짜를 알아내는 다른 방법으로는 프로그래밍 언어의 내장 함수를 사용하는 것이 있습니다. 예를 들어, Python의 "datetime" 모듈을 사용하면 현재 날짜 및 시간을 쉽게 출력할 수 있습니다.

## 관련 자료
- [GNU Coreutils](https://www.gnu.org/software/coreutils/coreutils.html)
- [Python datetime 모듈](https://docs.python.org/3/library/datetime.html)