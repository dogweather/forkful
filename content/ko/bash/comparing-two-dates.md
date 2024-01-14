---
title:    "Bash: 두 날짜 비교하기"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## 왜
두 날짜를 비교하는 것을 해야 하는 이유는 무엇일까요?
날짜를 비교하는 것은 자주 사용되는 프로그래밍 작업 중 하나입니다. 예를 들어, 우리는 특정 날짜에 어떤 일이 발생했는지 확인하거나 날짜 간의 차이를 계산하고 싶을 때 사용합니다.

## 어떻게
날짜를 비교하는 가장 간단한 방법은 `>`와 `<`를 사용하는 것입니다. 이러한 기호를 사용하여 두 날짜를 비교하면 해당 날짜가 이후에 있는지 또는 이전에 있는지를 알 수 있습니다. 예를 들어, 2020년 10월 1일과 2020년 11월 1일을 비교하면 `>` 기호를 사용하여 첫 번째 날짜가 더 이전인지 확인할 수 있습니다.

```
Bash
if [ "2020-10-01" > "2020-11-01" ]
then
    echo "첫 번째 날짜가 더 이전입니다."
fi
```

이 외에도 여러 가지 방법으로 날짜를 비교할 수 있습니다. 예를 들어, `date` 명령어를 사용하여 날짜와 시간을 비교할 수 있습니다. 또한 `test`나 `if` 문을 사용하여 조건문을 작성할 수도 있습니다.

```
Bash
if date -d "$START_DATE" > "$END_DATE"
then
    echo "시작 날짜가 더 이전입니다."
fi
```

## 깊이 파헤치기
날짜를 비교할 때 주의해야 할 몇 가지 사항이 있습니다. 첫 번째로, 날짜 형식이 지켜져야 합니다. 예를 들어, `YYYY-MM-DD` 형식이어야만 올바른 비교가 가능합니다. 또한, 두 날짜의 시간대가 같아야만 정확한 결과를 얻을 수 있습니다. 이를 위해 `date` 명령어의 `-d` 옵션을 사용하여 날짜를 동일한 시간대로 변환할 수 있습니다.

```
Bash
if date -d "$START_DATE" -d "${END_DATE}T00:00:00UTC"
then
    echo "시작 날짜와 종료 날짜의 시간대가 같습니다."
fi
```

또한, 여러 날짜를 동시에 비교해야 할 경우 배열을 사용하면 편리합니다. 이를 통해 보다 유연하게 날짜를 비교할 수 있습니다.

```
Bash
DATES=(2019-01-01 2020-01-01 2021-01-01)
for date in "${DATES[@]}"
do
    if [ "$date" < "2020-01-01" ]
    then
        echo "$date는 2020년 이전입니다."
    fi
done
```

## 참고 자료
- [Bash programming cheat sheet](https://devhints.io/bash)
- [GNU Bash manual](https://www.gnu.org/software/bash/manual/)
- [Comparing dates in Bash](https://www.beyondlinux.com/2018/06/22/comparing-dates-in-bash/)