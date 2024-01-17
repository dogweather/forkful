---
title:                "현재 날짜 가져오기"
html_title:           "Fish Shell: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 무엇과 왜? 
"현재 날짜 얻기"는 현재 날짜와 시간 정보를 얻는 것을 말합니다. 이는 프로그래머들이 자신의 코드에 현재 날짜를 포함하고 싶어할 때 매우 유용한 기능입니다.

## 방법: 
Fish Shell에서 현재 날짜를 얻는 방법은 매우 간단합니다. 다음과 같이 입력해보세요:

```
set date (date)
echo $date
```

위 코드를 실행하면 현재 날짜와 시간 정보가 출력됩니다. 예를 들어, 오늘 날짜가 2021년 3월 25일이라면 다음과 같은 결과가 나올 것입니다:

`Thu, Mar 25, 2021 10:00 AM`

## 자세히 알아보기: 
현재 날짜를 얻는 것은 우리가 생각하는 것보다 더 복잡한 과정을 거치는 것 같습니다. 사실, 다른 프로그래밍 언어에서도 많이 사용되는 표준 라이브러리 시스템 함수를 사용하여 현재 날짜를 얻습니다. 한가지 대안으로는 date 명령어를 사용하는 것입니다. 

Fish Shell은 본질적으로 현재 날짜와 시간 정보를 저장하는 변수를 생성합니다. 이 변수에는 다양한 형식의 날짜 정보가 포함됩니다. 이러한 구조는 매우 유용하지만, 상황에 맞게 적절한 변수를 선택하는 것은 중요합니다. 더 많은 정보는 Fish Shell 매뉴얼에서 찾아볼 수 있습니다.

## 관련 정보: 
- [Fish Shell 매뉴얼 (영문)](https://fishshell.com/docs/current/ "공식 Fish Shell 매뉴얼")
- [Bash Shell에서 현재 날짜 얻기 (한글)](https://codechacha.com/ko/get-current-date-time-in-bash-shell/ "Bash Shell에서 현재 날짜 얻는 방법 설명 블로그")