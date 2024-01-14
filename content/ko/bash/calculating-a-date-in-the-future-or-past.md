---
title:                "Bash: 미래나 과거의 날짜 계산하기"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

날짜를 미래나 과거로 계산하는 방법을 배우는 이유는 시간을 다루는 프로그램을 작성할 때 매우 유용하기 때문입니다. 예를 들어, 예약 시스템이나 인터넷 서비스에서 특정 날짜로부터 얼마나 지났는지를 계산하는 기능 등이 있을 수 있습니다.

## 어떻게 하나요?

날짜를 계산하기 위해 Bash 스크립트에서는 `date` 명령어를 사용할 수 있습니다. `date` 명령어에는 여러 옵션이 있지만 여기서는 다음 옵션들을 살펴보겠습니다.

- `+%Y`: 네 자리 연도 (e.g. 2021)
- `+%m`: 두 자리 월 (e.g. 01, 02, ..., 12)
- `+%d`: 두 자리 일 (e.g. 01, 02, ..., 31)

또는 `+%j` 옵션을 사용하여 해당 연도의 몇 번째 날인지를 나타낼 수도 있습니다. 이 외에도 시간, 요일 등을 나타내는 옵션들이 있으니 필요에 따라 사용하면 됩니다.

만약 오늘 날짜가 2021년 8월 25일이라면, 다음과 같이 입력하여 내일 날짜를 출력할 수 있습니다.

```Bash
date -d "tomorrow" "+%Y/%m/%d"
```

Output:
2021/08/26

날짜를 과거로 계산할 때에는 `ago` 키워드를 사용하여 몇 일 전, 몇 주 전 등을 계산할 수 있습니다. 예를 들어, 2주 전의 날짜를 출력하고 싶다면 다음과 같이 입력할 수 있습니다.

```Bash
date -d "2 weeks ago" "+%Y/%m/%d"
```

Output:
2021/08/11

## 깊이 파고드는 법

`date` 명령어를 사용하여 날짜를 계산하는 방법은 매우 다양합니다. 위에서 살펴본 것처럼 날짜, 시간, 요일 등 다양한 정보를 출력할 수 있으며, 옵션을 조합하여 원하는 결과를 얻을 수 있습니다.

또한 `date` 명령어와 함께 Bash 스크립트에서 논리적인 연산을 사용하여 날짜를 계산할 수도 있습니다. 예를 들어, 특정 날짜 이후로만 예약이 가능한 예약 시스템이 있다면 오늘 날짜와 비교하여 이후 날짜만 예약이 가능하도록 스크립트를 작성할 수 있습니다.

더 많은 옵션과 예제는 `date` 명령어의 공식 매뉴얼을 참고하시기 바랍니다.

## 더 알아보기

- `date` 명령어 공식 매뉴얼: https://www.gnu.org/software/coreutils/manual/html_node/touch-invocation.html
- Bash 스크립트 예제 모음: https://www.shellscript.sh/
- 날짜/시간 계산을 위한 다양한 Bash 패키지들: https://github.com/topics/bash-date