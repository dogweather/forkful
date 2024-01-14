---
title:    "Bash: 날짜를 문자열로 변환하기"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 변환하는 프로그래밍을 왜 해야 하는지 궁금하신가요? 날짜 간의 비교 및 정렬, 출력 형식을 바꿀 때 유용합니다.

## 어떻게

우선, ```date``` 명령어를 이용해 현재 날짜 정보를 출력합니다.

```Bash
date
```

다음으로, 원하는 형식으로 날짜를 출력하고 싶다면 ```+%Y-%m-%d```와 같은 옵션을 추가해주면 됩니다.

```Bash
date +%Y-%m-%d
```

예를 들어, 2021년 4월 25일이라는 형식으로 출력하고 싶다면 아래와 같이 입력하면 됩니다.

```Bash
date +%Y년 %m월 %d일
```

이렇게 나온 결과를 변수에 저장하고 싶다면, ```$```를 이용해 변수를 지정해주면 됩니다.

```Bash
date_str=$(date +%Y년 %m월 %d일)
```

이제 저장된 변수를 이용해 다양한 연산을 할 수 있습니다. 예를 들어, 특정 날짜와 비교하고 싶다면, 아래와 같은 코드를 작성할 수 있습니다.

```Bash
date1="2021-04-25"
date2=$(date +%Y-%m-%d)

if [ $date1 -eq $date2 ]; then
  printf "두 날짜가 같습니다."
else
  printf "두 날짜가 다릅니다."
fi
```

위 코드에서 ```-eq```는 두 값을 비교하는 ```Bash```의 연산자입니다. 또 다른 예시로, 원하는 날짜 형식으로 출력된 날짜를 파일명으로 사용하고 싶다면 아래와 같이 사용할 수 있습니다.

```Bash
touch $(date +%Y%m%d).txt
```

위 명령어는 해당 날짜의 파일을 생성합니다. 즉, 현재는 2021년 4월 25일이어서 ```20210425.txt``` 파일이 생성됩니다.

## 깊이있게 알아보기

```date``` 명령어는 시스템의 현재 날짜 및 시간 정보를 제공합니다. 따라서, ```date``` 명령어와 함께 사용할 수 있는 여러 옵션들을 적절히 활용하면 많은 기능을 구현할 수 있습니다. 예를 들어, 날짜와 시간 정보를 함께 출력하거나, 특정 시간대의 날짜를 출력할 수도 있습니다. 더 깊이있게 알아보고 싶다면, ```man date``` 명령어를 통해 더 많은 정보를 확인할 수 있습니다.

## 더 알아보기

- [GNU Coreutils - date](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html#date-invocation)
- [The Linux Documentation Project - Manipulating time and date in Linux](https://www.tldp.org/LDP/abs/html/x323.html)
- [Bash Hackers Wiki - External commands date](https://wiki.bash-hackers.org/commands/builtin/date)