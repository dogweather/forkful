---
title:    "Bash: 두 날짜 비교하기."
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜

날짜 비교를 하는 이유는 무엇일까요? 날짜는 우리 일상에서 매우 중요한 역할을 합니다. 우리는 날짜를 통해 언제 어떤 일이 발생했는지 알 수 있고, 계획을 세울 때에도 날짜를 중요한 요소로 고려합니다. 따라서 날짜를 비교하는 것은 우리가 일상에서 자주 다루게 되는 일입니다.

## 어떻게

날짜를 비교하는 방법에 대해 알아보겠습니다. Bash 프로그래밍을 이용하여 두 날짜를 비교하는 방법을 간단한 예제와 함께 살펴보겠습니다.

```Bash
#!/bin/bash

# 첫 번째 날짜 지정
date1=2021-01-01

# 두 번째 날짜 지정
date2=2020-12-31

# 날짜 비교
if [[ "$date1" > "$date2" ]]
then
  echo "첫 번째 날짜가 더 큽니다."
elif [[ "$date1" < "$date2" ]]
then
  echo "두 번째 날짜가 더 큽니다."
else
  echo "두 날짜가 같습니다."
fi
```

위 예제를 실행하면 "첫 번째 날짜가 더 큽니다."라는 메시지가 출력됩니다. 이처럼 Bash는 비교 연산자를 이용하여 간단하게 날짜를 비교할 수 있습니다.

## 깊이있게 살펴보기

날짜 비교를 좀 더 깊이있게 살펴보겠습니다. 날짜를 비교할 때에는 몇 가지 주의해야할 사항이 있습니다. 첫째, 날짜 형식이 일치해야합니다. 위 예제에서는 날짜를 "YYYY-MM-DD" 형식으로 지정했지만, 다른 형식을 사용할 경우 비교 연산이 정상적으로 이루어지지 않을 수 있습니다. 둘째, 날짜가 유효한 범위 내에 있는지 체크해야합니다. 만약 유효하지 않은 날짜를 비교하는 경우 오류가 발생할 수 있습니다. 따라서 날짜를 비교하는 경우에는 이러한 사항들을 고려하여 적절한 에러 처리를 해주는 것이 좋습니다.

## 이어보기

날짜 비교에 대해 더 알아보고 싶다면 아래 링크들을 참고하세요.

[GNU Bash](https://www.gnu.org/software/bash/)  
[Comparison Operators in Bash](https://www.tldp.org/LDP/abs/html/comparison-ops.html)  
[Validating Dates in Bash](https://www.linuxjournal.com/content/validating-dates-bash-script)

## 참고

[날짜를 비교하는 방법](https://www.youtube.com/watch?v=b5ywuMWujvg)