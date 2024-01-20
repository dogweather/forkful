---
title:                "미래 또는 과거의 날짜 계산하기"
html_title:           "Bash: 미래 또는 과거의 날짜 계산하기"
simple_title:         "미래 또는 과거의 날짜 계산하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

떠오르거나 지나가는 날짜 계산은 미래 혹은 과거의 특정 일시를 얻기 위한 것입니다. 이는 종종 일정된 작업을 시작하거나, 이벤트 기록을 관리하거나, 날짜 기반 로직을 실행하는 등의 프로그램 작성에 필요합니다.

## 어떻게:

먼저, `date` 명령어를 사용하여 현재 날짜 및 시간을 가져옵니다:

```Bash
date
```
이 명령어는 현재 시간을 반환합니다: Tue Sep 21 13:15:00 KST 2022

만일 3일 후의 날짜를 계산하려면 아래 명령어를 사용하면 됩니다:

```Bash
date -d "3 days"
```
이 명령어는 3일 후의 날짜를 반환합니다: Fri Sep 24 13:15:00 KST 2022 

마찬가지로 1주 전의 날짜를 계산하려면 다음과 같이 입력합니다:

```Bash
date -d "1 week ago"
```
이 명령어는 1주 전의 날짜를 반환합니다: Tue Sep 14 13:15:00 KST 2022 

## 딥 다이브:

`date` 명령어는 UNIX와 Linux 시스템에서 오랫동안 사용되어 왔으며, 이는 미래이나 과거의 특정한 시간을 계산하기 위한 가장 간편한 방법 중 하나입니다. 하지만 이 외에도 bash에서 다른 방법으로 시간을 계산할 수 있습니다. 예를 들어, 특정 시간 간격을 초로 변환하여 `date`에 추가하거나 빼는 방법이 있습니다.

특히, `date` 명령어는 환경 설정에 따라 출력 형식이 달라집니다. 그러므로 다른 언어 혹은 지역 설정에서는 날짜 형식이 변경될 수 있습니다. 

## 참고하면 좋을 자료:

[Linux Date Command](https://www.computerhope.com/unix/udate.htm): Linux date 명령어에 대한 더 많은 정보를 얻을 수 있는 사이트입니다. 

[Bash Date Examples](https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/): Bash에서 날짜 형식을 다루는 다양한 예제를 제공하는 사이트입니다. 

[Advanced Bash-Scripting Guide](http://tldp.org/LDP/abs/html/): Bash 스크립트에 대한 광범위한 정보를 제공하는 사이트입니다. 이 안에는 날짜 및 시간에 대한 좀 더 복잡한 처리 방법에 대한 정보도 포함되어 있습니다.