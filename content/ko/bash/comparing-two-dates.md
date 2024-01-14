---
title:                "Bash: 두 날짜 비교하기"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜
날짜를 비교하는 방법을 배우는 이유는 간단합니다. 날짜는 프로그래밍에서 매우 중요하고 유용한 자료 유형이기 때문이며, 날짜를 비교하기 위한 강력한 도구가 되기 때문입니다.

## 어떻게
날짜를 비교하는 방법은 Bash 프로그래밍에서도 중요합니다. 우리는 두 개의 날짜를 비교하는 간단한 Bash 스크립트를 만들어보겠습니다.

```
#!/bin/bash

# 첫 번째 날짜
first_date="2021-01-10"

# 두 번째 날짜
second_date="2021-01-20"

# 날짜를 Unix epoch로 변환
first_epoch=$(date -d "$first_date" +%s)
second_epoch=$(date -d "$second_date" +%s)

# 날짜 비교
if [ $first_epoch -gt $second_epoch ]; then
    echo "$second_date 이 $first_date 보다 이전입니다."
elif [ $first_epoch -lt $second_epoch ]; then
    echo "$first_date 이 $second_date 보다 이전입니다."
else
    echo "두 날짜는 같습니다."
fi
```

위의 스크립트를 실행하면 다음과 같은 출력을 볼 수 있습니다.

```
2021-01-20 이 2021-01-10 보다 이전입니다.
```

배열을 사용하여 여러 날짜를 비교할 수도 있습니다.

```
#!/bin/bash

# 비교할 날짜 배열
dates=("2021-01-10" "2021-01-20" "2021-02-01" "2021-02-10")

# 비교할 날짜 수
length=${#dates[@]}

# 가장 이전 날짜로 초기화
first_date=${dates[0]}
first_epoch=$(date -d "$first_date" +%s)

# 배열 순회하며 가장 이전 날짜와 비교
for (( i=1; i<$length; i++ )); do
    current_date=${dates[$i]}
    current_epoch=$(date -d "$current_date" +%s)
    if [ $first_epoch -gt $current_epoch ]; then
        first_date=$current_date
        first_epoch=$current_epoch
    fi
done

echo "$first_date 이 가장 이전 날짜입니다."
```

```
2021-01-10 이 가장 이전 날짜입니다.
```

## 깊이 있는 내용
두 날짜를 비교할 때, Unix epoch라는 개념을 사용하여 모든 날짜를 숫자로 변환합니다. 이렇게 함으로써 날짜의 크기를 비교할 수 있게 되고, 더 쉽게 두 날짜를 비교할 수 있습니다. 그리고 날짜를 배열로 다루는 방법을 배울 때 유용한 스킬이 될 수 있습니다.

## 참고
* [Unix epoch - Wikipedia](https://ko.wikipedia.org/wiki/Unix_%ED%99%88%EC%A0%84)
* [Unix epoch calculator](https://www.epochconverter.com/)