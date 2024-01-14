---
title:                "Fish Shell: 날짜를 문자열로 변환하기"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# 왜

날짜를 문자열로 변환하는 데 관심이 있는 이유는 무엇일까요? 이 글에서는 Fish Shell을 사용하여 날짜를 문자열로 변환하는 방법을 알려드리겠습니다. 날짜를 문자열로 바꾸는 기능은 매우 유용하며 효율적인 프로그래밍에 큰 도움이 될 수 있습니다.

## 사용 방법

```Fish Shell
set today (date +"%Y-%m-%d")
echo $today
```

위의 코드를 실행하면 현재 날짜를 "연-월-일" 형식의 문자열로 반환합니다. 이렇게 반환된 문자열을 원하는 형식으로 변경해 사용할 수도 있습니다. 예를 들어, "월-일-연도" 형식으로 바꾸려면 다음과 같이 코드를 작성할 수 있습니다.

```Fish Shell
set today (date +"%m-%d-%Y")
echo $today
```

출력 결과는 다음과 같이 나타납니다.

```
09-05-2020
```

이처럼 Fish Shell을 사용하면 매우 쉽게 날짜를 문자열로 변환할 수 있습니다.

## 깊이 있는 알아보기

Fish Shell에서 날짜를 문자열로 변환하는 과정은 날짜 값을 생성하는 date와 문자열을 반환하는 echo 명령어를 사용하는 것이 기본 원리입니다. 위에서 사용한 "%Y-%m-%d"와 "%m-%d-%Y"는 날짜 값을 년, 월, 일 형식으로 나타내는 지정자입니다. 더 많은 지정자를 사용할 수 있으며 해당하는 값을 자유롭게 조합하여 사용할 수 있습니다.

## 더 알아보기

- [Fish Shell 공식 홈페이지](https://fishshell.com/)
- [Fish Shell Github 저장소](https://github.com/fish-shell/fish-shell)
- [Fish Shell 설치 및 기본 사용법](https://blog.outsider.ne.kr/781)
- [날짜와 다른 형식으로 바꾸기](https://blog.outsider.ne.kr/784)

# 참고자료

- [Fish Shell Date and Time Command](https://www.shellhacks.com/fish-get-current-date-time/)
- [Fish Shell Date Formatting](https://fishshell.com/docs/current/cmds/date.html)