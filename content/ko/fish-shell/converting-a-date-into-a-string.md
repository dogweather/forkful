---
title:    "Fish Shell: 날짜를 문자열로 변환하는 방법"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## 왜: 날짜를 문자열로 변환하는 것에 대해 관심이 있는지

날짜를 다양한 형식으로 표현하고 싶을 때, 혹은 파일 생성이나 파일 라벨링 등에서 날짜 정보를 문자열로 사용하기 위해 일반적으로 날짜를 문자열로 변환하게 됩니다. 이를 위해 Fish Shell에서는 편리하게 날짜를 문자열로 변환할 수 있는 방법을 제공하고 있습니다.

## 사용 방법

날짜를 문자열로 변환하는 가장 간단한 방법은 `strftime` 함수를 사용하는 것입니다. 아래는 현재 날짜를 `%Y-%m-%d` 형식의 문자열로 변환하는 예시 코드입니다.

```Fish Shell
set currentDate (date +%Y-%m-%d)
echo $currentDate
```

위 코드를 실행하면 다음과 같이 출력됩니다.

```bash
2021-07-21
```

또 다른 예시로는 `%A, %B %d, %Y` 형식의 문자열로 변환하는 코드를 살펴보겠습니다.

```Fish Shell
set currentDate (date +"%A, %B %d, %Y")
echo $currentDate
```

이번에는 다음과 같이 출력됩니다.

```bash
Wednesday, July 21, 2021
```

## 깊게 파보기

`strftime` 함수는 날짜를 원하는 형식의 문자열로 변환하는 기능을 제공합니다. 사용 가능한 형식은 여러 가지가 있으며, 공식 문서를 참고하면 적절한 형식을 선택할 수 있습니다. 또한 `strptime` 함수를 사용하면 문자열을 날짜로 변환할 수도 있습니다.

하지만 날짜를 문자열로 변환하는 과정에서 원하는 형식 또는 특정한 지역에서 사용하는 날짜 표현 방식에 대한 이슈가 발생할 수 있습니다. 이러한 경우, 미리 설정한 언어 변수를 사용하여 문제를 해결할 수 있습니다.

한국어로 출력된 날짜 문자열을 영어로 바꾸기 위해선, 아래와 같은 코드를 사용할 수 있습니다.

```Fish Shell
set currentDate (LC_TIME="en_US.UTF-8" date +"%A, %B %d, %Y")
echo $currentDate
```

위 코드를 실행하면 다음과 같이 출력됩니다.

```bash
Wednesday, July 21, 2021
```

## 관련 링크

- Fish Shell 공식 문서: [Strftime](https://fishshell.com/docs/3.1/cmds/date.html#strftime)
- UNIX-like 시스템에서 날짜와 시간 표현하기: [Strftime와 strptime 함수](https://www.lesstif.com/software-architect/unix-like-strftime-strptime-12552428.html)