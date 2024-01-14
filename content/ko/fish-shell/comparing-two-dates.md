---
title:    "Fish Shell: 두 날짜 비교하기"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜

날짜를 서로 비교하는 것의 이유는 무엇일까요? 많은 사람들은 날짜를 비교하는 데에 익숙하지 않은데, 이는 작업에서 필요한 비교 로직을 사용하기 위한 중요한 도구입니다.

## 어떻게

먼저, 두 날짜를 어떻게 비교하는지 알아보겠습니다. Fish Shell을 사용하여 날짜를 비교하는 방법을 살펴봅시다. 

먼저, 날짜를 변수에 할당합니다.

```Fish Shell
set start_date 2020-01-15
set end_date 2020-01-20
```

다음으로, 날짜를 비교하기 위해 `date` 함수를 사용합니다.

```Fish Shell
date -f %F $start_date $end_date
```

위의 예제에서 사용된 `-f` 옵션은 날짜를 비교할 때 사용하는 형식을 지정하는 옵션입니다. `%F`는 날짜를 `yyyy-mm-dd` 형식으로 비교하도록 설정합니다.

또 다른 방법은 날짜를 `seconds`로 변환하여 비교하는 것입니다.

```Fish Shell
date -d $start_date +%s
date -d $end_date +%s
```

위의 예제에서 사용된 `-d` 옵션은 날짜를 `seconds`로 변환하는 옵션입니다. 이렇게 변환한 후에는 날짜를 숫자로 비교할 수 있습니다.

## 깊이 파고들기

날짜를 비교하는 것이 얼마나 중요한지 알고 계십니까? 많은 운영체제나 프로그래밍 언어에서는 날짜를 다룰 수 있는 기능을 제공하지만, Fish Shell에서도 간단하게 비교할 수 있습니다. 또한 날짜 형식을 변경하고 특정 날짜 간에 기간을 계산하는 등 유용한 기능을 제공합니다.

## 더 보기

- [Fish Shell 공식 문서](https://fishshell.com/docs/current/)
- [Linuxize: 날짜 비교하기](https://linuxize.com/post/how-to-compare-strings-in-bash/)
- [geekflare: Unix 날짜 함수 사용하기](https://geekflare.com/unix-date-command/)