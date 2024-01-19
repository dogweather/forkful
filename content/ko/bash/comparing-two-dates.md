---
title:                "두 날짜 비교하기"
html_title:           "C#: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 무엇이고 왜필요한가?

두 날짜를 비교한다는 것은 두 개의 시간 인스턴스를 비교하는 작업입니다. 프로그래머는 분석, 예약 시스템, 기간 트래킹 등에서 시간 간격이 필요하기 때문입니다.

## 어떻게?

두 날짜를 비교하는 가장 간단한 방법은 다음 코드를 사용하는 것입니다.

```Bash
# 첫 번째 날짜를 Unix epoch 시간으로 변환
date1=$(date -d "2021-10-02" +%s)

# 두 번째 날짜를 Unix epoch 시간으로 변환
date2=$(date -d "2021-11-02" +%s)

# 두 날짜를 비교
if [ $date1 -eq $date2 ]
then
   echo "날짜가 같습니다."
elif [ $date1 -lt $date2 ]
then
   echo "첫 번째 날짜가 더 이립니다."
else
   echo "두 번째 날짜가 더 이립니다."
fi
```
출력:
```
첫 번째 날짜가 더 이립니다.
```

## 깊이 들여다보기

Unix 시스템에서 시간은 1970년 1월 1일부터 초로 표현되는 epoch 시간으로 관리됩니다. 따라서 Bash에서 날짜 비교는 기본적으로 epoch 초로 변환 후 비교가 됩니다.

다른 방법으로 Awk, Perl, Python과 같은 언어를 사용하여 날짜를 관리할 수 있습니다. 이들 언어는 강력한 날짜와 시간 관리 기능을 제공합니다.

Bash 자체의 date 명령어로는 복잡한 날짜 계산이 제한적일 수 있습니다. 만약 복잡한 날짜/시간 계산을 수행해야하는 경우 해당 언어들을 사용해보는 것도 고려해볼 만 합니다.

## 참고자료

1. Unix epoch 시간에 대한 정보 - [여기](https://en.wikipedia.org/wiki/Unix_time)를 참고하십시오.
2. 언어별 날짜와 시간 처리 방법: Awk ([링크](https://www.gnu.org/software/gawk/manual/html_node/Time-Functions.html)), Perl ([링크](https://perldoc.perl.org/functions/localtime)), Python ([링크](https://docs.python.org/3/library/datetime.html))