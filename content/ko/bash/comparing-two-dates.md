---
title:                "두 날짜의 비교"
html_title:           "Bash: 두 날짜의 비교"
simple_title:         "두 날짜의 비교"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜

우리 모두는 어떤 시점과 다른 시점을 비교할 필요가 있을 때가 종종 있습니다. 이것은 일상 생활에서도 IT 업계에서도 빈번하게 발생합니다. 두 날짜를 비교하는 것은 시간과 날짜를 효율적으로 다룰 수 있는 중요한 기술입니다.

## 사용 방법

두 날짜를 비교하는 가장 기본적인 방법은 'date' 명령어와 조건문을 사용하는 것입니다.

```Bash
date1="2020-07-15"
date2="2020-07-30"
if [ "$date1" == "$date2" ]
then
  echo "두 날짜는 동일합니다."
else
  echo "두 날짜는 다릅니다."
fi
```

위의 예제에서는 date1과 date2를 각각 변수에 할당하고, 조건문을 통해 두 변수의 값이 동일한지 여부를 비교합니다. 두 날짜가 같다면 "두 날짜는 동일합니다."라는 메시지가 출력되고, 다르다면 "두 날짜는 다릅니다."라는 메시지가 출력됩니다.

또한, 날짜를 비교할 때는 비교 연산자를 사용할 수도 있습니다.

```Bash
date1="2020-07-15"
date2="2020-07-30"
if [ "$date1" -lt "$date2" ]
then
  echo "$date1보다 $date2가 더 나중입니다."
else
  echo "$date1보다 $date2가 더 빠릅니다."
fi
```

위의 예제에서는 두 날짜를 비교할 때 'lt'라는 비교 연산자를 사용합니다. 이는 "less than"을 의미하며, 첫번째 날짜(date1)가 두번째 날짜(date2)보다 더 빠른지 여부를 판단합니다. 위의 예제에서는 date1이 date2보다 빠른 경우에만 "date1보다 date2가 더 나중입니다."라는 메시지가 출력됩니다.

위의 예제들은 간단한 날짜 비교만을 다루었지만, 더 복잡한 비교를 위해서는 'date' 명령어의 옵션을 사용할 수도 있습니다. 예를 들어, 날짜 간의 차이를 구하는 것도 가능합니다.

```Bash
date1="2020-07-15"
date2="2020-07-30"
diff=$((($(date -d "$date2" +%s) - $(date -d "$date1" +%s)) / 86400))
echo "두 날짜의 차이는 $diff일입니다."
```

위의 예제에서는 먼저 'date' 명령어를 사용해 날짜를 UNIX timestamp 형태로 변환하고, 이를 이용해 두 날짜의 차이를 일 단위로 구합니다. 이 외에도 'date' 명령어에는 다양한 옵션을 제공하므로, 필요에 따라 적절한 옵션을 찾아서 사용할 수 있습니다.

## 깊이 파헤치기

두 날짜를 비교할 때, 윤년과 같은 엣지 케이스를 고려해야 할 필요도 있을 수 있습니다. 이 경우, 'date' 명령어의 '%j' 혹은 '%s' 옵션을 사용해 날짜를 Julian 날짜로 변경한 뒤 비교하는 것이 좋습니다.

또한, UNIX timestamp를 사용할 때는 주의해야