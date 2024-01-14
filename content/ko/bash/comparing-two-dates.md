---
title:                "Bash: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜

여러분께서 두 날짜를 비교하는 것에 관심을 가질 수 있는 이유는 무엇일까요? 날짜 비교는 주로 프로그래밍에서 사용되는 중요한 기능 중 하나입니다. 두 날짜를 비교하면서 날짜를 조작하고 계산할 수 있기 때문에, 프로그래밍을 할 때 유용하게 활용될 수 있습니다.

## 어떻게 하나요?

날짜를 비교하는 가장 간단한 방법은 `date` 명령어와 함께 사용하는 것입니다. 예를 들어, 2021년 4월 1일과 2021년 4월 30일을 비교하고 싶다면 다음과 같이 입력할 수 있습니다.

```Bash
if [[ 2021-04-01 > 2021-04-30 ]]; then
  echo "2021-04-01은 2021-04-30보다 나중입니다."
else
  echo "2021-04-01은 2021-04-30보다 빠릅니다."
fi
```

위 코드에서는 `>` 연산자를 사용하여 두 날짜를 비교했습니다. 결과는 `2021-04-01은 2021-04-30보다 빠릅니다.`가 출력됩니다. 이처럼 `date` 명령어를 활용하여 두 날짜를 비교할 수 있습니다.

## 더 깊이 파헤쳐 보기

하지만 실제로는 이보다 더 복잡한 날짜 비교가 필요할 때가 있습니다. 예를 들어, 같은 날짜인지, 몇 년 간 차이나는지 등을 비교해야 할 수 있습니다. 이 경우, `date` 명령어 외에도 `timedatectl` 명령어를 활용할 수 있습니다.

우선 `timedatectl` 명령어를 통해 시스템의 현재 시간 설정을 확인해 봅시다. 다음과 같이 입력합니다.

```Bash
timedatectl
```

출력에서 `Local time`, `Universal time` 등의 정보를 확인할 수 있습니다. 이 정보들을 활용하여 두 날짜를 비교할 수도 있습니다. 예를 들어, 다음과 같이 입력하면 현재 시간을 비교하여 오전/오후를 출력할 수 있습니다.

```Bash
if [[ $(timedatectl | grep "Local time" | cut -d " " -f 2) > $(timedatectl | grep "Local time" | cut -d " " -f 4) ]]; then
  echo "지금은 오후 시간입니다."
else
  echo "지금은 오전 시간입니다."
fi
```

위 코드에서는 `timedatectl` 명령어에서 `Local time` 정보를 추출하고, `cut` 명령어를 이용하여 시간 정보를 분리합니다. 그리고 `>` 연산자를 사용하여 두 시간을 비교하고, 결과에 따라 다른 메시지를 출력합니다.

## 다른 예제와 참고 자료들

- [Linuxize - How to Compare Dates in Bash](https://linuxize.com/post/bash-compare-dates/)
- [Unix StackExchange - How to Compare Two Dates](https://unix.stackexchange.com/questions/561971/how-to-compare-two-dates-in-terminal)
- [Medium - Bash Date Comparison Techniques](https://medium.com/developer-advocate-com/bash-date-comparison-techniques-4cd54fc4ec99)

---

## 참고 자료들

- [Linuxize - How to Compare Dates in Bash](https://linuxize.com/post/bash-compare-dates/)
- [Unix StackExchange - How to Compare Two Dates](https://unix.stackexchange.com/questions/561971/how-to-com