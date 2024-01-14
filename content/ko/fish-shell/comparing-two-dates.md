---
title:                "Fish Shell: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜 

두 날짜를 비교하는 것이 왜 중요한지 궁금해 하셨나요? 간단히 말하면, 두 날짜를 비교하면 더 효율적인 코드를 작성할 수 있고, 일정한 패턴을 찾는 데 도움이 됩니다. 또한 날짜와 시간은 많은 애플리케이션에서 중요한 역할을 하는 기능이므로, 정확히 비교하는 것이 매우 중요합니다.

## 사용 방법

Fish Shell을 사용하여 두 날짜를 비교하는 방법은 매우 간단합니다. 다음과 같은 형식으로 `date` 명령어를 사용합니다.

```
$ date -f '%Y-%m-%d' $first_date > $second_date # $first_date와 $second_date 변수에 각각 날짜를 저장
```

여기서 `-f` 플래그를 사용하여 날짜 형식을 지정해줍니다. `%Y`는 연도, `%m`은 월, `%d`는 일을 나타냅니다. 이 외에도 `%H`는 시간, `%M`은 분, `%S`는 초를 나타내는데 사용할 수 있습니다.

예를 들어, 2021년 7월 15일과 2021년 7월 20일을 비교하려면 다음과 같이 작성합니다.

```
$ date -f '%Y-%m-%d' 2021-07-15 > 2021-07-20
```

Output으로는 첫 번째 날짜가 두 번째 날짜보다 이전인 경우 `-1`을, 두 날짜가 같은 경우 `0`을, 첫 번째 날짜가 두 번째 날짜보다 이후인 경우 `1`을 반환합니다.

## 깊게 알아보기

날짜를 비교할 때 주의해야 할 점이 있습니다. 바로 윤년과 30일 미만의 월을 다루는 것입니다. 이러한 경우, `date` 명령어가 정확한 결과를 내지 못할 수 있습니다. 따라서 `%j`를 사용하여 날짜를 비교하지 않고, 일 수를 비교하는 것이 더 정확합니다.

이 외에도 `strftime`이라는 함수를 사용하여 날짜 형식을 바꿔주는 것도 가능합니다. 예를 들어, `%V`를 사용하면 ISO 8601 주 번호를 반환할 수 있습니다. 이러한 함수를 사용하면 더 정확하고 유용한 결과를 얻을 수 있습니다.

## See Also

- [Fish Shell 공식 홈페이지](https://fishshell.com/): Fish Shell의 공식 홈페이지로, 더 많은 정보와 사용 방법을 알아볼 수 있습니다.
- [Unix Timestamp 변환기](https://www.unixtimestamp.com): 날짜를 Unix Timestamp로 변환해주는 온라인 도구입니다. Unix Timestamp는 1970년 1월 1일부터 경과된 초를 나타내는 숫자 형식입니다.
- [Python으로 날짜 비교하는 방법](https://wikidocs.net/78561): 파이썬을 사용하여 날짜를 비교하는 방법에 대해 알아볼 수 있습니다. Fish Shell과 비슷한 형식을 사용하기 때문에 참고하기 좋습니다.