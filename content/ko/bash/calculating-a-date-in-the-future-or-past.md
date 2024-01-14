---
title:    "Bash: 미래나 과거의 날짜 계산하기"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

날짜를 미래나 과거로 계산하는 프로그래밍을 하는 이유는 여러 가지가 있을 수 있습니다. 예를 들어, 생일이나 기념일의 투표 일정이나 약속 등을 계획할 때 유용하게 사용할 수 있습니다.

## 사용 방법

날짜를 계산하는 방법은 매우 간단합니다. 먼저 `date` 명령어를 사용하여 현재 시간과 날짜를 확인합니다. 그리고 나서 `date` 옵션을 설정하여 원하는 날짜를 지정해줍니다. 예를 들어, 다음과 같은 명령어를 입력하면 내일의 날짜를 확인할 수 있습니다.

```Bash
date -d "tomorrow"
```

만약 미래나 과거의 특정 날짜를 계산하려면, 추가적으로 `+`나 `-`를 사용하여 해당 날짜를 지정해주면 됩니다. 예를 들어, 30일 전의 날짜를 계산하려면 다음과 같이 입력하면 됩니다.

```Bash
date -d "-30 days"
```

또한, 시간 단위를 지정하여 계산하는 것도 가능합니다. 예를 들어, 1시간 30분 전의 날짜와 시간을 계산하려면 다음과 같이 입력하면 됩니다.

```Bash
date -d "-1 hour -30 minutes"
```

## 깊이있게 알아보기

Bash에서 날짜를 계산하는 데에는 여러 가지 옵션을 사용할 수 있습니다. 위에서는 `date` 명령어를 사용하였지만, `cal`이나 `calcurse` 등의 다른 명령어를 사용하여도 같은 결과를 얻을 수 있습니다.

또한, `date` 명령어는 여러 가지 옵션을 지정하여 사용할 수 있습니다. 예를 들어, 원하는 형식으로 날짜를 출력하고 싶을 때는 `-u` 옵션을 사용하여 UTC 시간을 출력하거나, `-I` 옵션을 사용하여 ISO 8601 형식으로 출력할 수 있습니다.

더 나아가서, Bash에서는 날짜를 계산할 때마다 자주 사용되는 변수들도 정의되어 있습니다. `date` 명령어에서 `-d` 옵션을 사용한 경우 해당 날짜가 `$1`, `$2` 등의 변수로 저장되어 있습니다. 이를 활용하여 반복문과 조건문 등을 사용하여 보다 복잡한 계산도 가능합니다.

## 관련 정보

* [Unix Man Page - date](https://www.unix.com/man-page/linux/1/date/)
* [GNU Coreutils - Date](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
* [TLDP - Date and Time Calculation](https://tldp.org/LDP/abs/html/datecalc.html)